/* Top-level LTO routines.
   Copyright (C) 2009-2019 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "function.h"
#include "bitmap.h"
#include "basic-block.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "tree-streamer.h"
#include "cgraph.h"
#include "opts.h"
#include "toplev.h"
#include "stor-layout.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "debug.h"
#include "lto.h"
#include "lto-section-names.h"
#include "splay-tree.h"
#include "lto-partition.h"
#include "context.h"
#include "pass_manager.h"
#include "ipa-fnsummary.h"
#include "ipa-utils.h"
#include "gomp-constants.h"
#include "lto-symtab.h"
#include "stringpool.h"
#include "fold-const.h"
#include "attribs.h"
#include "builtins.h"
#include "lto-common.h"


/* Number of parallel tasks to run, -1 if we want to use GNU Make jobserver.  */
static int lto_parallelism;

/* Return true when NODE has a clone that is analyzed (i.e. we need
   to load its body even if the node itself is not needed).  */

static bool
has_analyzed_clone_p (struct cgraph_node *node)
{
  struct cgraph_node *orig = node;
  node = node->clones;
  if (node)
    while (node != orig)
      {
	if (node->analyzed)
	  return true;
	if (node->clones)
	  node = node->clones;
	else if (node->next_sibling_clone)
	  node = node->next_sibling_clone;
	else
	  {
	    while (node != orig && !node->next_sibling_clone)
	      node = node->clone_of;
	    if (node != orig)
	      node = node->next_sibling_clone;
	  }
      }
  return false;
}

/* Read the function body for the function associated with NODE.  */

static void
lto_materialize_function (struct cgraph_node *node)
{
  tree decl;

  decl = node->decl;
  /* Read in functions with body (analyzed nodes)
     and also functions that are needed to produce virtual clones.  */
  if ((node->has_gimple_body_p () && node->analyzed)
      || node->used_as_abstract_origin
      || has_analyzed_clone_p (node))
    {
      /* Clones don't need to be read.  */
      if (node->clone_of)
	return;
      if (DECL_FUNCTION_PERSONALITY (decl) && !first_personality_decl)
	first_personality_decl = DECL_FUNCTION_PERSONALITY (decl);
      /* If the file contains a function with a language specific EH
	 personality set or with EH enabled initialize the backend EH
	 machinery.  */
      if (DECL_FUNCTION_PERSONALITY (decl)
	  || opt_for_fn (decl, flag_exceptions))
	lto_init_eh ();
    }

  /* Let the middle end know about the function.  */
  rest_of_decl_compilation (decl, 1, 0);
}

/* Materialize all the bodies for all the nodes in the callgraph.  */

static void
materialize_cgraph (void)
{
  struct cgraph_node *node;
  timevar_id_t lto_timer;

  if (!quiet_flag)
    fprintf (stderr,
	     flag_wpa ? "Materializing decls:" : "Reading function bodies:");


  FOR_EACH_FUNCTION (node)
    {
      if (node->lto_file_data)
	{
	  lto_materialize_function (node);
	  lto_stats.num_input_cgraph_nodes++;
	}
    }


  /* Start the appropriate timer depending on the mode that we are
     operating in.  */
  lto_timer = (flag_wpa) ? TV_WHOPR_WPA
	      : (flag_ltrans) ? TV_WHOPR_LTRANS
	      : TV_LTO;
  timevar_push (lto_timer);

  current_function_decl = NULL;
  set_cfun (NULL);

  if (!quiet_flag)
    fprintf (stderr, "\n");

  timevar_pop (lto_timer);
}

/* Actually stream out ENCODER into TEMP_FILENAME.  */

static void
stream_out (char *temp_filename, lto_symtab_encoder_t encoder, int part)
{
  lto_file *file = lto_obj_file_open (temp_filename, true);
  if (!file)
    fatal_error (input_location, "%<lto_obj_file_open()%> failed");
  lto_set_current_out_file (file);

  gcc_assert (!dump_file);
  streamer_dump_file = dump_begin (TDI_lto_stream_out, NULL, part);
  ipa_write_optimization_summaries (encoder);

  free (CONST_CAST (char *, file->filename));

  lto_set_current_out_file (NULL);
  lto_obj_file_close (file);
  free (file);
  if (streamer_dump_file)
    {
      dump_end (TDI_lto_stream_out, streamer_dump_file);
      streamer_dump_file = NULL;
    }
}

/* Wait for forked process and signal errors.  */
#ifdef HAVE_WORKING_FORK
static void
wait_for_child ()
{
  int status;
  do
    {
#ifndef WCONTINUED
#define WCONTINUED 0
#endif
      int w = waitpid (0, &status, WUNTRACED | WCONTINUED);
      if (w == -1)
	fatal_error (input_location, "waitpid failed");

      if (WIFEXITED (status) && WEXITSTATUS (status))
	fatal_error (input_location, "streaming subprocess failed");
      else if (WIFSIGNALED (status))
	fatal_error (input_location,
		     "streaming subprocess was killed by signal");
    }
  while (!WIFEXITED (status) && !WIFSIGNALED (status));
}
#endif

static void
stream_out_partitions_1 (char *temp_filename, int blen, int min, int max)
{
   /* Write all the nodes in SET.  */
   for (int p = min; p < max; p ++)
     {
       sprintf (temp_filename + blen, "%u.o", p);
       stream_out (temp_filename, ltrans_partitions[p]->encoder, p);
       ltrans_partitions[p]->encoder = NULL;
     }
}

/* Stream out ENCODER into TEMP_FILENAME
   Fork if that seems to help.  */

static void
stream_out_partitions (char *temp_filename, int blen, int min, int max,
		       bool ARG_UNUSED (last))
{
#ifdef HAVE_WORKING_FORK
  static int nruns;

  if (lto_parallelism <= 1)
    {
      stream_out_partitions_1 (temp_filename, blen, min, max);
      return;
    }

  /* Do not run more than LTO_PARALLELISM streamings
     FIXME: we ignore limits on jobserver.  */
  if (lto_parallelism > 0 && nruns >= lto_parallelism)
    {
      wait_for_child ();
      nruns --;
    }
  /* If this is not the last parallel partition, execute new
     streaming process.  */
  if (!last)
    {
      pid_t cpid = fork ();

      if (!cpid)
	{
	  setproctitle ("lto1-wpa-streaming");
          stream_out_partitions_1 (temp_filename, blen, min, max);
	  exit (0);
	}
      /* Fork failed; lets do the job ourseleves.  */
      else if (cpid == -1)
	stream_out_partitions_1 (temp_filename, blen, min, max);
      else
	nruns++;
    }
  /* Last partition; stream it and wait for all children to die.  */
  else
    {
      int i;
      stream_out_partitions_1 (temp_filename, blen, min, max);
      for (i = 0; i < nruns; i++)
	wait_for_child ();
    }
  asm_nodes_output = true;
#else
  stream_out_partitions_1 (temp_filename, blen, min, max);
#endif
}

/* Write all output files in WPA mode and the file with the list of
   LTRANS units.  */

static void
lto_wpa_write_files (void)
{
  unsigned i, n_sets;
  ltrans_partition part;
  FILE *ltrans_output_list_stream;
  char *temp_filename;
  auto_vec <char *>temp_filenames;
  auto_vec <int>temp_priority;
  size_t blen;

  /* Open the LTRANS output list.  */
  if (!ltrans_output_list)
    fatal_error (input_location, "no LTRANS output list filename provided");

  timevar_push (TV_WHOPR_WPA);

  FOR_EACH_VEC_ELT (ltrans_partitions, i, part)
    lto_stats.num_output_symtab_nodes
    += lto_symtab_encoder_size (part->encoder);

  timevar_pop (TV_WHOPR_WPA);

  timevar_push (TV_WHOPR_WPA_IO);

  cgraph_node *node;
  /* Do body modifications needed for streaming before we fork out
     worker processes.  */
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    if (!node->clone_of && gimple_has_body_p (node->decl))
      lto_prepare_function_for_streaming (node);

  ggc_trim ();
  report_heap_memory_use ();

  /* Generate a prefix for the LTRANS unit files.  */
  blen = strlen (ltrans_output_list);
  temp_filename = (char *) xmalloc (blen + sizeof ("2147483648.o"));
  strcpy (temp_filename, ltrans_output_list);
  if (blen > sizeof (".out")
      && strcmp (temp_filename + blen - sizeof (".out") + 1,
		 ".out") == 0)
    temp_filename[blen - sizeof (".out") + 1] = '\0';
  blen = strlen (temp_filename);

  n_sets = ltrans_partitions.length ();
  unsigned sets_per_worker = n_sets;
  if (lto_parallelism > 1)
    {
      if (lto_parallelism > (int)n_sets)
	lto_parallelism = n_sets;
      sets_per_worker = (n_sets + lto_parallelism - 1) / lto_parallelism;
    }

  for (i = 0; i < n_sets; i++)
    {
      ltrans_partition part = ltrans_partitions[i];

      /* Write all the nodes in SET.  */
      sprintf (temp_filename + blen, "%u.o", i);

      if (!quiet_flag)
	fprintf (stderr, " %s (%s %i insns)", temp_filename, part->name,
		 part->insns);
      if (symtab->dump_file)
	{
	  lto_symtab_encoder_iterator lsei;

	  fprintf (symtab->dump_file,
		   "Writing partition %s to file %s, %i insns\n",
		   part->name, temp_filename, part->insns);
	  fprintf (symtab->dump_file, "  Symbols in partition: ");
	  for (lsei = lsei_start_in_partition (part->encoder);
	       !lsei_end_p (lsei);
	       lsei_next_in_partition (&lsei))
	    {
	      symtab_node *node = lsei_node (lsei);
	      fprintf (symtab->dump_file, "%s ", node->asm_name ());
	    }
	  fprintf (symtab->dump_file, "\n  Symbols in boundary: ");
	  for (lsei = lsei_start (part->encoder); !lsei_end_p (lsei);
	       lsei_next (&lsei))
	    {
	      symtab_node *node = lsei_node (lsei);
	      if (!lto_symtab_encoder_in_partition_p (part->encoder, node))
		{
		  fprintf (symtab->dump_file, "%s ", node->asm_name ());
		  cgraph_node *cnode = dyn_cast <cgraph_node *> (node);
		  if (cnode
		      && lto_symtab_encoder_encode_body_p (part->encoder,
							   cnode))
		    fprintf (symtab->dump_file, "(body included)");
		  else
		    {
		      varpool_node *vnode = dyn_cast <varpool_node *> (node);
		      if (vnode
			  && lto_symtab_encoder_encode_initializer_p (part->encoder,
								      vnode))
			fprintf (symtab->dump_file, "(initializer included)");
		    }
		}
	    }
	  fprintf (symtab->dump_file, "\n");
	}
      gcc_checking_assert (lto_symtab_encoder_size (part->encoder) || !i);

      temp_priority.safe_push (part->insns);
      temp_filenames.safe_push (xstrdup (temp_filename));
    }
  memory_block_pool::trim (0);

  for (int set = 0; set < MAX (lto_parallelism, 1); set++)
    {
      stream_out_partitions (temp_filename, blen, set * sets_per_worker,
			     MIN ((set + 1) * sets_per_worker, n_sets),
			     set == MAX (lto_parallelism, 1) - 1);
    }

  ltrans_output_list_stream = fopen (ltrans_output_list, "w");
  if (ltrans_output_list_stream == NULL)
    fatal_error (input_location,
		 "opening LTRANS output list %s: %m", ltrans_output_list);
  for (i = 0; i < n_sets; i++)
    {
      unsigned int len = strlen (temp_filenames[i]);
      if (fprintf (ltrans_output_list_stream, "%i\n", temp_priority[i]) < 0
	  || fwrite (temp_filenames[i], 1, len, ltrans_output_list_stream) < len
	  || fwrite ("\n", 1, 1, ltrans_output_list_stream) < 1)
	fatal_error (input_location, "writing to LTRANS output list %s: %m",
		     ltrans_output_list);
     free (temp_filenames[i]);
    }

  lto_stats.num_output_files += n_sets;

  /* Close the LTRANS output list.  */
  if (fclose (ltrans_output_list_stream))
    fatal_error (input_location,
		 "closing LTRANS output list %s: %m", ltrans_output_list);

  free_ltrans_partitions ();
  free (temp_filename);

  timevar_pop (TV_WHOPR_WPA_IO);
}

/* Perform whole program analysis (WPA) on the callgraph and write out the
   optimization plan.  */

static void
do_whole_program_analysis (void)
{
  symtab_node *node;

  lto_parallelism = 1;

  /* TODO: jobserver communication is not supported, yet.  */
  if (!strcmp (flag_wpa, "jobserver"))
    lto_parallelism = param_max_lto_streaming_parallelism;
  else
    {
      lto_parallelism = atoi (flag_wpa);
      if (lto_parallelism <= 0)
	lto_parallelism = 0;
      if (lto_parallelism >= param_max_lto_streaming_parallelism)
	lto_parallelism = param_max_lto_streaming_parallelism;
    }

  timevar_start (TV_PHASE_OPT_GEN);

  /* Note that since we are in WPA mode, materialize_cgraph will not
     actually read in all the function bodies.  It only materializes
     the decls and cgraph nodes so that analysis can be performed.  */
  materialize_cgraph ();

  /* Reading in the cgraph uses different timers, start timing WPA now.  */
  timevar_push (TV_WHOPR_WPA);

  if (pre_ipa_mem_report)
    dump_memory_report ("Memory consumption before IPA");

  symtab->function_flags_ready = true;

  if (symtab->dump_file)
    symtab->dump (symtab->dump_file);
  bitmap_obstack_initialize (NULL);
  symtab->state = IPA_SSA;

  execute_ipa_pass_list (g->get_passes ()->all_regular_ipa_passes);

  /* When WPA analysis raises errors, do not bother to output anything.  */
  if (seen_error ())
    return;

  /* We are about to launch the final LTRANS phase, stop the WPA timer.  */
  timevar_pop (TV_WHOPR_WPA);

  /* We are no longer going to stream in anything.  Free some memory.  */
  lto_free_file_name_hash ();


  timevar_push (TV_WHOPR_PARTITIONING);

  gcc_assert (!dump_file);
  dump_file = dump_begin (partition_dump_id, NULL);

  if (dump_file)
    symtab->dump (dump_file);

  symtab_node::checking_verify_symtab_nodes ();
  bitmap_obstack_release (NULL);
  if (flag_lto_partition == LTO_PARTITION_1TO1)
    lto_1_to_1_map ();
  else if (flag_lto_partition == LTO_PARTITION_MAX)
    lto_max_map ();
  else if (flag_lto_partition == LTO_PARTITION_ONE)
    lto_balanced_map (1, INT_MAX);
  else if (flag_lto_partition == LTO_PARTITION_BALANCED)
    lto_balanced_map (param_lto_partitions,
		      param_max_partition_size);
  else
    gcc_unreachable ();

  /* Size summaries are needed for balanced partitioning.  Free them now so
     the memory can be used for streamer caches.  */
  ipa_free_size_summary ();

  /* AUX pointers are used by partitioning code to bookkeep number of
     partitions symbol is in.  This is no longer needed.  */
  FOR_EACH_SYMBOL (node)
    node->aux = NULL;

  lto_stats.num_cgraph_partitions += ltrans_partitions.length ();

  /* Find out statics that need to be promoted
     to globals with hidden visibility because they are accessed from multiple
     partitions.  */
  lto_promote_cross_file_statics ();
  if (dump_file)
     dump_end (partition_dump_id, dump_file);
  dump_file = NULL;
  timevar_pop (TV_WHOPR_PARTITIONING);

  timevar_stop (TV_PHASE_OPT_GEN);

  /* Collect a last time - in lto_wpa_write_files we may end up forking
     with the idea that this doesn't increase memory usage.  So we
     absoultely do not want to collect after that.  */
  ggc_collect ();

  timevar_start (TV_PHASE_STREAM_OUT);
  if (!quiet_flag)
    {
      fprintf (stderr, "\nStreaming out");
      fflush (stderr);
    }
  lto_wpa_write_files ();
  if (!quiet_flag)
    fprintf (stderr, "\n");
  timevar_stop (TV_PHASE_STREAM_OUT);

  if (post_ipa_mem_report)
    dump_memory_report ("Memory consumption after IPA");

  /* Show the LTO report before launching LTRANS.  */
  if (flag_lto_report || (flag_wpa && flag_lto_report_wpa))
    print_lto_report_1 ();
  if (mem_report_wpa)
    dump_memory_report ("Final");
}

/* Create artificial pointers for "omp declare target link" vars.  */

static void
offload_handle_link_vars (void)
{
#ifdef ACCEL_COMPILER
  varpool_node *var;
  FOR_EACH_VARIABLE (var)
    if (lookup_attribute ("omp declare target link",
			  DECL_ATTRIBUTES (var->decl)))
      {
	tree type = build_pointer_type (TREE_TYPE (var->decl));
	tree link_ptr_var = make_node (VAR_DECL);
	TREE_TYPE (link_ptr_var) = type;
	TREE_USED (link_ptr_var) = 1;
	TREE_STATIC (link_ptr_var) = 1;
	SET_DECL_MODE (link_ptr_var, TYPE_MODE (type));
	DECL_SIZE (link_ptr_var) = TYPE_SIZE (type);
	DECL_SIZE_UNIT (link_ptr_var) = TYPE_SIZE_UNIT (type);
	DECL_ARTIFICIAL (link_ptr_var) = 1;
	tree var_name = DECL_ASSEMBLER_NAME (var->decl);
	char *new_name
	  = ACONCAT ((IDENTIFIER_POINTER (var_name), "_linkptr", NULL));
	DECL_NAME (link_ptr_var) = get_identifier (new_name);
	SET_DECL_ASSEMBLER_NAME (link_ptr_var, DECL_NAME (link_ptr_var));
	SET_DECL_VALUE_EXPR (var->decl, build_simple_mem_ref (link_ptr_var));
	DECL_HAS_VALUE_EXPR_P (var->decl) = 1;
      }
#endif
}

unsigned int
lto_option_lang_mask (void)
{
  return CL_LTO;
}

/* Main entry point for the GIMPLE front end.  This front end has
   three main personalities:

   - LTO (-flto).  All the object files on the command line are
     loaded in memory and processed as a single translation unit.
     This is the traditional link-time optimization behavior.

   - WPA (-fwpa).  Only the callgraph and summary information for
     files in the command file are loaded.  A single callgraph
     (without function bodies) is instantiated for the whole set of
     files.  IPA passes are only allowed to analyze the call graph
     and make transformation decisions.  The callgraph is
     partitioned, each partition is written to a new object file
     together with the transformation decisions.

   - LTRANS (-fltrans).  Similar to -flto but it prevents the IPA
     summary files from running again.  Since WPA computed summary
     information and decided what transformations to apply, LTRANS
     simply applies them.  */

void
lto_main (void)
{
  /* LTO is called as a front end, even though it is not a front end.
     Because it is called as a front end, TV_PHASE_PARSING and
     TV_PARSE_GLOBAL are active, and we need to turn them off while
     doing LTO.  Later we turn them back on so they are active up in
     toplev.c.  */
  timevar_pop (TV_PARSE_GLOBAL);
  timevar_stop (TV_PHASE_PARSING);

  timevar_start (TV_PHASE_SETUP);

  /* Initialize the LTO front end.  */
  lto_fe_init ();

  timevar_stop (TV_PHASE_SETUP);
  timevar_start (TV_PHASE_STREAM_IN);

  /* Read all the symbols and call graph from all the files in the
     command line.  */
  read_cgraph_and_symbols (num_in_fnames, in_fnames);

  timevar_stop (TV_PHASE_STREAM_IN);

  if (!seen_error ())
    {
      offload_handle_link_vars ();

      /* If WPA is enabled analyze the whole call graph and create an
	 optimization plan.  Otherwise, read in all the function
	 bodies and continue with optimization.  */
      if (flag_wpa)
	do_whole_program_analysis ();
      else
	{
	  timevar_start (TV_PHASE_OPT_GEN);

	  materialize_cgraph ();
	  if (!flag_ltrans)
	    lto_promote_statics_nonwpa ();

	  /* Annotate the CU DIE and mark the early debug phase as finished.  */
	  debuginfo_early_start ();
	  debug_hooks->early_finish ("<artificial>");
	  debuginfo_early_stop ();

	  /* Let the middle end know that we have read and merged all of
	     the input files.  */
	  symtab->compile ();

	  timevar_stop (TV_PHASE_OPT_GEN);

	  /* FIXME lto, if the processes spawned by WPA fail, we miss
	     the chance to print WPA's report, so WPA will call
	     print_lto_report before launching LTRANS.  If LTRANS was
	     launched directly by the driver we would not need to do
	     this.  */
	  if (flag_lto_report || (flag_wpa && flag_lto_report_wpa))
	    print_lto_report_1 ();
	}
    }

  /* Here we make LTO pretend to be a parser.  */
  timevar_start (TV_PHASE_PARSING);
  timevar_push (TV_PARSE_GLOBAL);
}
