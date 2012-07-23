/* Definitions for the shared dumpfile.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#ifndef GCC_DUMPFILE_H
#define GCC_DUMPFILE_H 1

/* Different tree dump places.  When you add new tree dump places,
   extend the DUMP_FILES array in tree-dump.c.  */
enum tree_dump_index
{
  TDI_none,			/* No dump */
  TDI_cgraph,                   /* dump function call graph.  */
  TDI_tu,			/* dump the whole translation unit.  */
  TDI_class,			/* dump class hierarchy.  */
  TDI_original,			/* dump each function before optimizing it */
  TDI_generic,			/* dump each function after genericizing it */
  TDI_nested,			/* dump each function after unnesting it */
  TDI_vcg,			/* create a VCG graph file for each
				   function's flowgraph.  */
  TDI_ada,                      /* dump declarations in Ada syntax.  */
  TDI_tree_all,                 /* enable all the GENERIC/GIMPLE dumps.  */
  TDI_rtl_all,                  /* enable all the RTL dumps.  */
  TDI_ipa_all,                  /* enable all the IPA dumps.  */

  TDI_end
};

/* Bit masks to control dumping. Not all values are applicable to
   all dumps. Add new ones at the end. When you define new
   values, extend the DUMP_OPTIONS array in tree-dump.c */
#define TDF_ADDRESS	(1 << 0)	/* dump node addresses */
#define TDF_SLIM	(1 << 1)	/* don't go wild following links */
#define TDF_RAW  	(1 << 2)	/* don't unparse the function */
#define TDF_DETAILS	(1 << 3)	/* show more detailed info about
					   each pass */
#define TDF_STATS	(1 << 4)	/* dump various statistics about
					   each pass */
#define TDF_BLOCKS	(1 << 5)	/* display basic block boundaries */
#define TDF_VOPS	(1 << 6)	/* display virtual operands */
#define TDF_LINENO	(1 << 7)	/* display statement line numbers */
#define TDF_UID		(1 << 8)	/* display decl UIDs */

#define TDF_TREE	(1 << 9)	/* is a tree dump */
#define TDF_RTL		(1 << 10)	/* is a RTL dump */
#define TDF_IPA		(1 << 11)	/* is an IPA dump */
#define TDF_STMTADDR	(1 << 12)	/* Address of stmt.  */

#define TDF_GRAPH	(1 << 13)	/* a graph dump is being emitted */
#define TDF_MEMSYMS	(1 << 14)	/* display memory symbols in expr.
                                           Implies TDF_VOPS.  */

#define TDF_DIAGNOSTIC	(1 << 15)	/* A dump to be put in a diagnostic
					   message.  */
#define TDF_VERBOSE     (1 << 16)       /* A dump that uses the full tree
					   dumper to print stmts.  */
#define TDF_RHS_ONLY	(1 << 17)	/* a flag to only print the RHS of
					   a gimple stmt.  */
#define TDF_ASMNAME	(1 << 18)	/* display asm names of decls  */
#define TDF_EH		(1 << 19)	/* display EH region number
					   holding this gimple statement.  */
#define TDF_NOUID	(1 << 20)	/* omit UIDs from dumps.  */
#define TDF_ALIAS	(1 << 21)	/* display alias information  */
#define TDF_ENUMERATE_LOCALS (1 << 22)	/* Enumerate locals by uid.  */
#define TDF_CSELIB	(1 << 23)	/* Dump cselib details.  */
#define TDF_SCEV	(1 << 24)	/* Dump SCEV details.  */
#define TDF_COMMENT	(1 << 25)	/* Dump lines with prefix ";;"  */


/* In tree-dump.c */

extern char *get_dump_file_name (int);
extern int dump_enabled_p (int);
extern int dump_initialized_p (int);
extern FILE *dump_begin (int, int *);
extern void dump_end (int, FILE *);
extern void dump_node (const_tree, int, FILE *);
extern int dump_switch_p (const char *);
extern const char *dump_flag_name (int);

/* Global variables used to communicate with passes.  */
extern FILE *dump_file;
extern int dump_flags;
extern const char *dump_file_name;

/* Return the dump_file_info for the given phase.  */
extern struct dump_file_info *get_dump_file_info (int);

/* Define a tree dump switch.  */
struct dump_file_info
{
  const char *suffix;           /* suffix to give output file.  */
  const char *swtch;            /* command line switch */
  const char *glob;             /* command line glob  */
  int flags;                    /* user flags */
  int state;                    /* state of play */
  int num;                      /* dump file number */
};


#endif /* GCC_DUMPFILE_H */
