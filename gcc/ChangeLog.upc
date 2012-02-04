2012-02-03  Nenad Vukicevic  <nenad@intrepid.com>

	* c-family/c-pragma.c (disable_pupc_mode): New. Disable profiling
	code generation (same as #pragma pupc off).
	(set_pupc_mode): New. Set/restore profiling mode.
	* c-family/c-upc.h (disable_pupc_mode): New. Prototype.
	(set_pupc_mode): New. Prototype.
	* upc/upc-act.c (upc_write_init_func): Disable emitting of the
	profiling code for shared variables initialization routines. 

2012-01-31  Gary Funck  <gary@intrepid.com>

	Merge trunk version 183751 into gupc branch.
	Incorporates fix for bootstrap failure on openSUSE 12.1.

2012-01-10  Gary Funck  <gary@intrepid.com>

	Merge trunk version 183072 into gupc branch.
	Incorporates libcpp __BASE_FILE__ fix.

2011-12-20  Nenad Vukicevic <nenad@intrepid.com>

	* top-level/Makefile.def (flags_to_pass): Added GUPC
	defines. Fixes the make error when upc is not specified as one
	of the languages to build.
	* top-level/Makefile.in: Re-generate.

2011-11-22  Gary Funck  <gary@intrepid.com>

	* DEV-PHASE: "GCC UPC" -> "GNU UPC", and bump minor rev.
	* dwarf2out.c (gen_compile_unit_die): Check for
	the "GNU UPC" language string in lieu of "GCC UPC".
	* config/rs6000/rs6000.c (rs6000_output_function_epilogue): Ditto.

2011-11-18  Gary Funck  <gary@intrepid.com>

	Merge trunk version 181552 into gupc branch.
	Incorporates libgcc/libunwind fix for IA64.

2011-11-19  Gary Funck  <gary@intrepid.com>

	* config/rs6000/rs6000.c (rs6000_output_function_epilogue):
	Add check for UPC when defining the language type value
	in a traceback entry.

2011-11-18  Gary Funck  <gary@intrepid.com>

	Merge trunk version 181493 into gupc branch.
	Incorporates final fix for PR target/49992.

2011-11-09  Nenad Vukicevic <nenad@intrepid.com>

	* Makefile.in (CRTSTUFF_CFLAGS): Revert the previous
	change that converted relative include paths into absolute
	ones, as it does not work with older version of make (3.80).

2011-10-27  Nenad Vukicevic <nenad@intrepid.com>

	Apply patch for Darwin build - PR49992.
	* top-level/configure.ac: Don't run ranlib with '-c' option for Darwin.
	* gcc/configure.ac: Ditto.
	* gcc/ada/mlib-tgt-specific-darwin.adb: Ditto.
	* gcc/ada/gcc-interface/Makefile.in: Ditto.
	* top-level/configure: Re-generate.
	* gcc/configure: Re-genrate.

2011-10-26  Gary Funck  <gary@intrepid.com>

	Rename "GCC/UPC" to "GNU UPC", "UPC" to "GUPC", and
	"libupc" to "libgupc".
	* top-level/configure.ac: Implement support for cross-builds, and
	  adjust for rename of libupc to libgupc.
	* top-level/Makefile.tpl: Ditto.
	* top-level/Makefile.def: Ditto.
	* top-level/configure: Re-generate.
	* top-level/Makefile.in: Re-generate.
	* top-level/contrib/gcc_update: Adjust for rename of libupc to libgupc.
	* doc/tm.texi.in: Adjust for rename of libupc to libgupc.
	* doc/tm.texi: Re-generate.
	* tree-pretty-print.c (dump_block_node): Delete unused variable.
	* gcc.c: Adjust for rename of libupc to libgupc.
	* testsuite/lib/upc.exp: Adjust for rename of libupc to libgupc.
	Change "GCC_UNDER_TEST" to "GUPC_UNDER_TEST".
	Change "xupc" to "xgupc".
	* configure.ac: Change "GCC UPC" to "GNU UPC".
	* Makefile.in (CRTSTUFF_CFLAGS): Adjust $(INCLUDES_FOR_TARGET)
	so that they are absolute paths.  This is needed because
	the upc-crtstuff builds are in the libgupc library build
	directories which are not at the same level as libgcc.
	* config/darwin.h: Adjust for rename of libupc to libgupc.

2011-10-20  Gary Funck  <gary@intrepid.com>

	Merge trunk version 180276 into gupc branch.
	Incorporates fix for PR bootstrap/50709.

2011-10-20  Gary Funck  <gary@intrepid.com>

	Merge trunk version 180246 into gupc branch.

2011-10-19  Gary Funck  <gary@intrepid.com>

	Merge trunk version 180233 into gupc branch.
	Incorporates fix for PR debug/49310 (var tracking).

2011-10-11  Nenad Vukicevic <nenad@intrepid.com>

	* testsuite/lib/upc-dg.exp: Limit number of torture runs to
	only four (O0 static/dynamic, O3 static/dynamic).
	Detect -fupc-threads-0 as an option for dynamic threads
	compile environment.

2011-10-10  Gary Funck  <gary@intrepid.com>

	* tree.c (check_qualified_type, check_aligned_type):
	Call tree_int_cst_equal() to compare UPC blocking factors
	if the corresponding tree pointers are not equal.
	* c-typeck.c (comptypes_internal, c_build_qualified_type_1): Ditto.

2011-10-07  Nenad Vukicevic <nenad@intrepid.com>

	Add configuration checks for struct/packed builds so we can
	use upc_struct_pts/upc_packed_pts selectors in the testsuite.
	* testsuite/lib/target-supports.exp
	(check_effective_target_upc_struct_pts): New.
	(check_effective_target_upc_packed_pts): New.

2011-09-15  Nenad Vukicevic <nenad@intrepid.com>

	Add to FLAGS the flags needed to disable inlining of
	UPC run-time access routines.
	* testsuite/lib/target-supports.exp
	(add_options_for_upc_library_calls): New.

2011-09-15  Gary Funck  <gary@intrepid.com>

	Fix ICE involving shared bit field accesses.
	* tree.c (build3_stat): Propagate TEEE_SHARED()
	and TREE_STRICT() and TREE_RELAXED() flags, if applicable.

2011-09-13  Gary Funck  <gary@intrepid.com>

	Merge trunk version 178795 into gupc branch.
	Incorporates fix to PR bootstrap/50010 for x86-32.

2011-09-08  Gary Funck  <gary@intrepid.com>

	Merge trunk version 178557 into gupc branch.

2011-09-07  Gary Funck  <gary@intrepid.com>

	Ensure that UPC pointer-to-shared type alignment is
	propagated to the final type.  Revert to long-standing
	alignment policy: twice the size of a "C" pointer.
	* tree.c (build_pointer_type): Propagate the alignment
	of the UPC pointer-to-shared representation type
	into the newly built pointer type.

2011-08-30  Gary Funck  <gary@intrepid.com>

	* tree.h (check_qualified_type): Change 'const_tree'
	argument types back to 'tree' to avoid complaints
	of assignment drops qualifiers for invocations of the
	newly implemented TYPE_BLOCK_FACTOR() macro, which
	invokes hash functions with 'tree' pointer values that
	are not const qualified.
	* tree.c (check_qualified_type, check_aligned_type): Ditto.
	* c-typeck.c (comptypes_internal): Ditto.

2011-08-29  Gary Funck  <gary@intrepid.com>

	Fixes for regressions noted running "make check"
	versus GCC trunk.
	* fold-const.c (fold_unary_loc): Execute UPC-specific
	checks for a cast-of-a-cast, only if one of the
	operands is a UPC pointer-to-shared value.
	* opts.c (print_specific_help):  Fix a comparison
	in an assert that checks that there are enough
	bits reserved for the number of languages defined
	in the .opt files.

2011-08-29  Gary Funck  <gary@intrepid.com>

	Implement a hash table to record UPC block factors.
	* c-family/stub-upc.c (upc_block_factor_insert,
	upc_block_factor_lookup): New dummy stub procedures.
	* c-family/c-common.c (c_sizeof_or_alignof_type):
	Rename UPC_TYPE_HAS_THREADS_FACTOR() to TYPE_HAS_THREADS_FACTOR().
	* tree.c (copy_node_stat): call SET_TYPE_BLOCK_FACTOR()
	to copy (hashed) UPC blocking factor.
	(set_type_quals): Likewise.
	* tree.h: Rename UPC_TYPE_HAS_THREADS_FACTOR() to
	TYPE_HAS_THREADS_FACTOR().
	(type_common.block_factor): Delete.
	(TYPE_HAS_BLOCK_FACTOR_0, TYPE_HAS_BLOCK_FACTOR_X,
	TYPE_HAS_BLOCK_FACTOR, SET_TYPE_BLOCK_FACTOR): New.
	(TYPE_BLOCK_FACTOR): Re-implement, using hash table for
	UPC blocking factors greater than one.
	* dwarf2out.c (modified_type_die): Re-implement
	logic that records UPC blocking factor in the generated
	DWARF2 debugging information.
	(add_subscript_info): Rename UPC_TYPE_HAS_THREADS_FACTOR() to
	TYPE_HAS_THREADS_FACTOR().
	* c-decl.c (finish_decl, grokdeclarator):
	Rename UPC_TYPE_HAS_THREADS_FACTOR() to TYPE_HAS_THREADS_FACTOR().
	* print-tree.c (print_node): Check for TYPE_LANG_FLAG_* flags
	used by UPC, and print UPC-specific information.

2011-08-28  Gary Funck  <gary@intrepid.com>

	Re-work the type machinery to fully support and to unify support
	for the UPC layout qualifier (blocking factor).
	* c-family/stub-upc.c (upc_set_block_factor): Delete.
	  (upc_grok_layout_qualifier): rename from upc_apply_layout_qualifier()
	  and update prototype.
	* c-family/c-common.c (complete_array_type): call newly
	  defined c_build_qualified_type_1() instead of
	  upc_set_block_factor().
	* c-family/c-upc.h (pc_grok_layout_qualifier): rename from
	  upc_apply_layout_qualifier() and update prototype.
	  (upc_set_block_factor): Delete prototype.
	* c-family/c-common.h (c_build_qualified_type): redefine
	  as a pre-processor macro that invokes c_build_qualified_type_1()
	  with a null UPC layout qualifier.
	  (c_build_qualified_type_1): New.  Add layout qualifier argument
	  to old c_build_qualified_type() prototype.
	* tree.c (set_type_quals, check_qualified_type):
	  Add layout qualifier as argument.
	  (check_aligned_type): Add check for UPC block factor equality.
	  (get_qualified_type_1): Rename from get_qualified_type() and
	  add layout qualifier as argument.
	  (build_qualified_type_1): Rename from build_qualified_type() and
	  add layout qualifier as argument.
	* tree.h (check_qualified_type): Add layout qualifier as argument
	  to the prototype.
	  (get_qualified_type): Re-define as a as a pre-processor macro
	  that invokes get_qualified_type_1() with a null
	  UPC layout qualifier.
	  (get_qualified_type_1): Rename from get_qualified_type() and
	  add layout qualifier as argument.
	* cp/tree.c (c_build_qualified_type_1): Rename from
	  c_build_qualified_type() and add (unused) layout qualifier
	  argument.
	* c-decl.c (finish_decl): re-format long error messages.
	  (grokdeclarator): Re-work the logic so that it calls
	  upc_grok_layout_qualifier() to handle the UPC layout
	  qualifier, if present.
	* c-typeck.c (qualify_type, common_pointer_type,
	  build_component_ref): Re-work the logic so that it calls
	  upc_grok_layout_qualifier() to handle the UPC layout qualifier,
	  if present.
	  (c_build_qualified_type_1): Rename from c_build_qualified_type()
	  and add layout qualifier argument.
	* print_tree.c (print_node): Re-format a long line.

2011-08-26  Gary Funck  <gary@intrepid.com>

	Merge trunk version 177949 into gupc branch.
	* c-family/c-common.h (enum rid): Bump the number of type modifiers
	  in the comment.
	* ../libcpp/init.c (struct lang_flags): Adjust the entry for UPC
	  to add new rliterals column value.

2011-08-26  Gary Funck  <gary@intrepid.com>

	* convert.c (convert_to_integer): Fix the check for the
	  difference between two UPC pointers-to-shared values.

2011-08-23  Nenad Vukicevic <nenad@intrepid.com>

	* configure.ac: Fix the name for --with-upc-pts-packed-bits
	  option. Support --with-upc-packed-bits as deprecated. Fix the code to
	  correctly check packed bits.
	* configure: Re-generate.

2011-08-16  Gary Funck  <gary@intrepid.com>

	* c-parser.c (c_parser_upc_sync_statement): issue error diagnostic if
	  the barrier id expression is not an integer expression.

2011-08-12  Gary Funck  <gary@intrepid.com>

	* config/i386/i386.h (MAX_FIXED_MODE_SIZE): Delete
	  (revert to trunk).
	* upc/upc-pts-struct.c (upc_pts_struct_init_type):
	  Set mode of UPC pointer-to-shared rep. to an
	  integral mode of size at least as large as the
	  size of the representation's struct type.

2011-08-12  Gary Funck  <gary@intrepid.com>

	Rework/simplify the UPC genericize pass.
	* c-family/stub-upc.c (upc_apply_layout_qualifier,
	  upc_build_pointer_type): New.
	  (upc_set_block_factor): Adjust to new calling sequence.
	  (upc_build_shared_var_addr): Delete.
	* c-family/c-common.c (complete_array_type): Adjust call
	  to upc_set_block_factor().
	* c-family/c-upc.h (upc_apply_layout_qualifier,
	  upc_build_pointer_type): New.
	  (upc_build_shared_var_addr): Delete.
	  (upc_set_block_factor): Adjust to new calling sequence.
	* tree.h (TI_UPC_CHAR_PTS_TYPE, upc_char_pts_type_node): New.
	* c-decl.c (grokdeclarator): Call newly defined
	  upc_apply_layout_qualifier() instead of upc_set_block_factor().
	* c-decl.c (grokdeclarator): Split long UPC-related error
	  messages into two lines.
	* c-typeck.c (build_unary_op): Do not call upc_build_shared_var_addr()
	  directly.  Revert that change to trunk. (upc_genericize() will
	  handle lowering the expressions that take the address of a
	  UPC variable).
	* config/i386/i386.c (ix86_promote_function_mode): For UPC
	  pointers-to-shared, return the type mode of the UPC pointer-to-shared
	  representation type.

2011-08-10  Gary Funck  <gary@intrepid.com>

	Implement additional fixes for recent merge with trunk.
	* explow.c (promote_mode): Do not attempt to promote
	  the mode for UPC pointer-to-shared types.
	* config/i386/i386.c (function_value_64): Ditto.

2011-08-07  Gary Funck  <gary@intrepid.com>

	Merge trunk version 177548 into gupc branch.
	* config/i386/i386.c (ix86_promote_function_mode):
	  Do not promote UPC pointers-to-shared to Pmode.
	* c-family/c-ommon.c (pointer_int_sum):
	  Also check the pointer operand, rather than just
	  its type, when making the decision to derive
	  the equivalent unshared type.
	* tree.c (build2_stat): If the type of the result
	  passed in as an argument is a UPC shared type,
	  derive the unshared equivalent when calculating
	  the result type of the node.

2011-07-15  Nenad Vukicevic <nenad@intrepid.com>

	* config/darwin.c: Disable var_tracking option
	  on -O0 as it is supposed to run only when optimization
	  is applied. See GCC bug 49743.

2011-07-06  Gary Funck  <gary@intrepid.com>

	Re-implement the pass that lowers trees generated by
	the UPC front-end into GENERIC.  Previously this was
	done within the gimplification framework, but this
	required that gimplification be run before inlining
	and various other passes, which did not fit in well
	with the current design of the middle-end passes.
	Now, the lowering is done by a newly defined language
	specific genericize hook.  With this change, we are
	are able to undo some extensions made to the gimplify
	logic, and thus can revert several files to trunk.

	* c-decl.c: Add a check for UPC deprecated names which
	  may appear as undefined function names.
	* libfuncs.h (LTI_upc_barrier, LTI_upc_notify, LTI_upc_wait,
	  LTI_upc_getaddr): Remove definitions of unused UPC
	  library function names.  These were defined back when
	  the libfunc interface was used to call these routines.
	* timevar.def (TV_TREE_UPC_GENERICIZE): New. Track time spent
	  in UPC lowering (genericize) pass.
	* langhooks.h (gimplify_expr): Revert to trunk's definition.
	  (instrument_func): Delete this hook.  Now handled in
	  upc_genericize().
	* tree-pass.h: Revert to trunk.
	* c-typeck.c (build_unary_op): Adjust reference to
	  upc_genericize() in a comment.
	* gimplify.c (create_tmp_var_raw): Delete logic that
	  was converting UPC shared types into unshared types
	  to be used for temporaries.  Now handled in  upc_genericize().
	  Replace with an assertion check.
	  (prepare_gimple_addressable): Revert to trunk's definition.
	  No longer needed as an externally called function.
	  (flag_instrument_functions_exclude_p): Re-define as
	  an externally called function.
	  (gimplify_function_tree): Delete code that implemented
	  call to UPC's language specific function instrumentation hook.
	* gimple.h (flag_instrument_functions_exclude_p): Define as extern.
	  (prepare_gimple_addressable): Delete extern definition;
	  revert to trunk's definition.
	* lnaghooks-def.h (lhd_gimplify_expr): Revert to trunk's definition.
	  (LANG_HOOKS_INSTRUMENT_FUNC): Remove, no longer needed for UPC.
	* c-family/c-gimplify.c: Revert to trunk.
	* cp/cp-gimplify.c: Revert to trunk.
	* cp/cp-tree.h: Revert to trunk.
	* explow.c: Revert to trunk.
	* langhooks.c: Revert to trunk.
	* libfuncs.h: Revert to trunk.
	* objc/objc-act.c: Revert to trunk.

2011-06-30  Gary Funck  <gary@intrepid.com>

	* c-family/c.opt: Bring options definitions up-to-date
	with respect to changes made in the trunk.

2011-06-29  Gary Funck  <gary@intrepid.com>

	Merge trunk version 175584 into gupc branch.

2011-06-13  Gary Funck  <gary@intrepid.com>

libcpp/
	* include/cpplib.h (enum c_lang):
	Move the entry for CLK_UPC so that it follows CLK_STDC1X.
	This keeps all the "C" variants together.
	* init.c (lang_defaults): Add an entry for UPC.

2011-06-09  Gary Funck  <gary@intrepid.com>

	* c-parser.c (upc_localsizeof_type): Fix an ICE when compiling for
	a 32-bit target, the size expression overflowed, the result was
	error_mark_node, and an ICE was triggered on an attempt to evaluate
	C_TYPE_VARIABLE_SIZE() on the error node.
	(upc_blocksizeof_expr, upc_blocksizeof_type,
	upc_elemsizeof_expr, upc_elemsizeof_type, upc_localsizeof_expr):
	Make similar changes to avoid further processing of error_mark_node.

2011-06-07  Gary Funck  <gary@intrepid.com>

	* sel-sched.c (move_op): Incorporate the following
	patch to eliminate ICE at -O3 on IA64/Altix.

2011-06-07  Alexander Monakov  <amonakov@ispras.ru>

	* sel-sched.c (move_op): Use correct type for 'res'.  Verify that
	code_motion_path_driver returned 0 or 1.
	(sel_region_finish): Clear h_d_i_d.

2011-06-02  Gary Funck  <gary@intrepid.com>

	Merge trunk version 174558 into gupc branch.

2011-05-30  Gary Funck  <gary@intrepid.com>

	* configure.ac: Make the default order of the virtual
	  address field in a UPC pointer-to-shared to be "first".
	  This reverts to the previous setting, and is a short term
	  measure to work around a bug found on the IA64,
	  where vaddr=last led to incorrect code generation.
	  Rename UPC_PTS_VADDR_FIRST to HAVE_UPC_PTS_VADDR_FIRST
	  in AC_DEFINE(), to agree with recent changes in files
	  that reference this define.
	  configure, config.in: Regenerate.

2011-05-26  Gary Funck  <gary@intrepid.com>

	* configure.ac: Fix typo when referring to $upc_vaddr_order.
	  configure: Regenerate.

	* ../fixincludes/fixincl.x: Revert to trunk.
	  This file is auto-generated, and should not be merged.

2011-05-18  Gary Funck  <gary@intrepid.com>

	Merge trunk version 173845 into gupc branch.

2011-05-17  Gary Funck  <gary@intrepid.com>

	* c-family/stub-upc.c (upc_rts_forall_depth_var): New.
	  c-family/c-upc.h (upc_rts_forall_depth_var): Define.

	* c-family/c-pragma.c: Remove conditional compilation
	  with HANDLE_PRAGMA_UPC and HANDLE_PRAGMA_PUPC.
	  Test 'compiling_upc' when compiling to determine if
	  the "upc" and "pupc" pragmas should be registered.

	* defaults.h (UPC_SHARED_SECTION_NAME, UPC_SHARED_BEGIN_NAME,
	  UPC_SHARED_END_NAME, UPC_PGM_INFO_SECTION_NAME,
	  UPC_PGM_INFO_BEGIN_NAME, UPC_PGM_INFO_END_NAME,
	  UPC_INIT_SECTION_NAME, UPC_INIT_BEGIN_NAME,
	  UPC_INIT_END_NAME, UPC_INIT_ARRAY_SECTION_NAME,
	  UPC_INIT_ARRAY_BEGIN_NAME, UPC_INIT_ARRAY_END_NAME): New.
	  Move these target-dependent definitions from config/upc-conf.h
	  to here.

	* configure.ac: Improve the logic for UPC-related options.
	  Delete references to pre-processor definitions that have
	  been moved to "upc/upc-pts.h".

	* configure, config.in: Regenerate.

	* Makefile.in (UPC_PTS_REP): Remove definition and revert
	  to trunk.  This substitution variable was used to
	  configure the representation-specific versions of
	  the tree rewrites that operate on UPC pointer-to-shared
	  types and objects.

	* c-parser.c (c_parser_upc_forall_statement): Remove
	  reference to UPC_FORALL_DEPTH_NAME, and call
	  newly defined upc_rts_forall_depth_var() instead.

	* config/upc-config.h: Delete. Various definitions
	  moved to "upc/upc-rts-names.h", "defaults.h", and
	  "upc/upc-pts.h".

	* doc/tm.texi.in (HAVE_UPC_PTS_VADDR_FIRST,
	  HAVE_UPC_PTS_PACKED_REP, HAVE_UPC_PTS_STRUCT_REP,
	  UPC_SHARED_SECTION_NAME, UPC_SHARED_BEGIN_NAME,
	  UPC_SHARED_END_NAME, UPC_PGM_INFO_SECTION_NAME,
	  UPC_PGM_INFO_BEGIN_NAME, UPC_PGM_INFO_END_NAME,
	  UPC_INIT_SECTION_NAME, UPC_INIT_BEGIN_NAME,
	  UPC_INIT_END_NAME, UPC_INIT_ARRAY_SECTION_NAME,
	  UPC_INIT_ARRAY_BEGIN_NAME, UPC_INIT_ARRAY_END_NAME):
	  New.  Document UPC target macros.
	  doc/tm.texi: Regenerate.

2011-05-07  Gary Funck  <gary@intrepid.com>

	* ../configure.ac: Disable build of libupc
	  on non POSIX hosted systems.  Use AS_HELP_STRING
	  to define messages.  Remove 'word-pair' as a
	  possible --with-upc-pts UPC pointer-to-shared
	  representation.

	* ../configure: Regenerate.

	* configure.ac: Use AS_HELP_STRING to define messages.
	  Remove 'word-pair' as a possible --with-upc-pts
	  UPC pointer-to-shared representation.
	  (UPC_MAX_THREADS): Limit the maximum value to 2^31-1.
	  (UPC_MAX_BLOCK_SIZE): Correct the default value.

	* configure: Regenerate.

	* config.in: Regenerate. Delete UPC_PTS_WORD_PAIR_REP
	  definition.

	* ChangeLog.upc: Fix some typos.

2011-05-07  Gary Funck  <gary@intrepid.com>

	* ../maintainer-scripts/gcc_release: Add "upc"
	  as one of the released languages.

2011-05-06  Gary Funck  <gary@intrepid.com>

	Eliminate compilation warnings, by fixing
	#include's and updating function prototypes.

	* c-family/c-cppbuiltin.c: Include c-upc.h.

	* optabs.c (gen_libfunc): Change type of 'suffix'
	  parameter to conform with prototype.

	* cp/cp-gimplify.c (cp_gimplify_expr): Add extra parameters
	  ('gimple_test_f' and 'fallback') used by extended gimplify_expr
	  hook used by UPC.

	* cp/cp-tree.h (cp_gimplify_expr): Add extra parameters to
	  the prototype.

	* objc/objc-act.c (objc_gimplify_expr): Pass extra dummy
	  argument values to cp_gimplify_expr.

	* config.in (HAVE_UPC_AFFINITY_SUPPORT,
	  HAVE_UPC_NUMA_SUPPORT): Regenerate.  Remove
	  pre-processor definitions that are no longer
	  needed to build the 'upc' command (upc-cmd.c)
	  because the linker specs. defined in libupc
	  take care of linking in the needed libraries.

	* c-parser.c (c_parser_upc_forall_statement):
	  initialize affinity_loc to avoid "maybe unused" warning.
	  (c_parser_upc_sync_statement): Remove un-needed
	  'ret' variable.  Cast return value from
	  'upc_build_sync_stmt' to 'void' to avoid
	  compile-time warning.

	* config/upc-conf.h (UPC_MAX_THREADS): Define as an
	  integer constant, not a string.  Range is restricted
	  to maximum positive 32-bit integer (2+ billion) to
	  fit in with the use of 'int' in the front-end's
	  switch handling logic.

2011-05-06  Gary Funck  <gary@intrepid.com>

	Upgrade c-family source files to conform with modularity
	improvements.  Mainly, remove #include of c-tree.h in files
	under c-family, and define a new UPC-specific #include file,
	c-upc.h, and use it.

	* c-family/stub-upc.c: Remove #include of c-tree.h and
	  upc/upc-act.h.  Replace with #include of c-common.h
	  and c-upc.h.
	  (upc_get_unshared_type, upc_pts_cvt_op_p, upc_blocksizeof_expr,
	  upc_blocksizeof_type, upc_elemsizeof_expr, upc_elemsizeof_type,
	  upc_localsizeof_expr, upc_localsizeof_type,
	  upc_shared_type_p): Delete.

	* c-family/c-opts.c: Add #include of c-upc.h

	* c-family/c-common.c: Remove #include of c-tree.h and
	  add #include of c-upc.h.

	* c-family/c-upc.h: New. Define API for UPC-specific functions
	  (mostly implemented in upc/upc-act.c).

	* c-family/c-common.h (upc_cpp_builtins, upc_write_global_declarations):
	  Remove extern definitions.

	* c-family/c-pragma.c: Remove #include of c-tree.h.
	  Add #include of c-upc.h.

	* tree.h (UPC_TYPE_HAS_THREADS_FACTOR): New.  Move from
	  c-tree.h.
	  (upc_shared_type_p, upc_pts_cvt_op_p): New.  Move from
	  upc/upc-act.c, and define as a macro.
	  (expand_affinity_test): Remove unused external definition.
	  (build_upc_unshared_type): Add external definition.
	  (upc_shared_type_p): Remove external definition.

	* c-config-lang.in: Update gtfiles to refer to c-family/c-upc.h.

	* dojump.c: Remove #include of c-tree.h.

	* c-tree.h: Remove definition of UPC_TYPE_HAS_THREADS_FACTOR
	  and move to tree.h.
	  (count_upc_threads_refs, is_multiple_of_upc_threads,
	  set_upc_threads_refs_to_one, c_expr, upc_affinity_test,
	  upc_build_shared_var_addr, upc_build_sync_stmt,
	  upc_check_decl_init, upc_check_decl, upc_decl_init, c_expr,
	  upc_get_block_factor, upc_instrument_forall, upc_is_null_pts_p,
	  c_expr, upc_num_threads, upc_diagnose_deprecated_stmt,
	  upc_pts_cvt_op_p, upc_pts_diff, upc_pts_increment,
	  upc_pts_int_sum, upc_set_block_factor, upc_set_decl_section,
	  permit_pragma_upc, deny_pragma_upc, pragma_upc_permitted_p,
	  set_upc_consistency_mode, get_upc_consistency_mode,
	  push_upc_consistency_mode, pop_upc_consistency_mode,
	  get_upc_pupc_mode):
	  Move external definitions to c-family/c-upc.h.
	  (upc_blocksizeof_type, upc_localsizeof_type,
	  upc_elemsizeof_type): Remove external definitions;
	  these functions were moved to c-parser.c.

	* c-decl.c: Add #include of c-upc.h.

	* c-typeck.c: Add #include of c-upc.h.

	* c-convert.c: Add #include of c-upc.h.

	* ChangeLog.upc: Fix typo.

	* Makefile.in: Add references to c-family/c-upc.h, everywhere
	  there is a reference to c-family/c-objc.h.
	  Remove extraneous reference to upc-act.h.

	* c-parser.c: Add #include of c-upc.h.
	  (upc_blocksizeof_expr, upc_blocksizeof_type,
	  upc_elemsizeof_expr, upc_elemsizeof_type,
	  upc_localsizeof_expr, upc_localsizeof_type):
	  Move from upc/upc-act.c.

	* tree.c (build_upc_unshared_type): New.
	  Move upc_get_unshared_type from upc/upc-act.c and rename
	  to build_upc_unshared_type.
	  c-family/c-common.c (pointer_int_sum): refer to renamed
	  build_upc_unshared_type function.
	  c-convert.c (convert): Ditto.
	  convert.c (convert_to_pointer): Ditto.
	  c-typeck.c (build_unary_op, build_modify_expr,
	  really_start_incremental_init): Ditto.
	  gimplify.c (create_tmp_var_raw): Ditto.
	  tree.c (build1_stat): Ditto.
	  tree.h (upc_get_unshared_type): Rename to
	  build_upc_unshared_type.

2011-05-06  Gary Funck  <gary@intrepid.com>

	Merge trunk version 173471 into gupc branch.

2011-05-05  Gary Funck  <gary@intrepid.com>

	Make changes that bring the GUPC branch more closely in sync.
	with the GCC trunk.  Revert any fixes that are not UPC-specific.
	Remove gratuitous re-formatting.

	* ../libstdc++-v3/config/os/bionic/ctype_noninline.h: Delete.
	  This file should have been removed in a previous merge
	  with the trunk.
	* ../configure.ac: Remove Cray Catamount/CNL support.
	* ../configure: Regenerate.
	* ../config.sub: Remove Cray Catamount/CNL support.
	* config.gcc: Remove Cray Catamount/CNL support.
	* ../maintainer-scripts/gcc_release: Revert to trunk.
	* c-family/c-common.h: Define parse_optimize_options.  Its definition
	  was missed in a previous merge with the trunk.
	* dwarf2out.c: Revert a fix which removed the 'type_main_variant'
	  procedure.
	* dwarf2out.c: Revert a fix that added a check for VECTOR_TYPE
	  in addition to ARRAY_TYPE.
	* c-typeck.c: Remove an extra newline character.
	* varasm.c: Revert a fix that improved an error message
	  when TLS common data is unimplemented.
	* varasm.c: Revert a gcc_assert that had been added which
	  checked for a null DECL_SIZE_UNIT field.
	* emultls.c: Revert to trunk. Remove possible fix.
	* Makefile.in: Revert extra blank line that is present
	  in the trunk version.
	* Makefile.in: Revert a fix that handled long shell
	  argument lists for plugin headers.
	* config/ia64/ia64.opt: Revert an option setting
	  that increased the default TLS address range.
	* config/ia64/crtbegin.asm: Revert to trunk.
	  Removes an extra newline character.
	* ChangeLog.upc: Spell check.

2011-05-03  Gary Funck  <gary@intrepid.com>

	* c-family/c-common.c: Remove extraneous FIXME/TODO comments.
	  (c_apply_type_quals_to_decl): Ditto.
	* c-family/c-common.h: Ditto.
	* dojump.c: Ditto.
	* c-decl.c (merge_decls): Ditto.
	* c-typeck.c (qualify_type, default_conversion,
	  build_component_ref, build_unary_op,
	  c_build_qualified_type): Ditto.
	* gimplify.c (create_tmp_var_raw): Remove UPC-specific dead code.

2011-04-29  Gary Funck  <gary@intrepid.com>

	* c-decl.c (finish_decl): Improve error diagnostics.
	  (grokdeclarator): Ditto.

	* c-typeck.c (build_c_cast): Improve error diagnostics.
	  (convert_for_assignment): Ditto.
	  (build_binary_op): Ditto.

	* c-parser.c (c_parser_upc_forall_statement):
	  Improve error diagnostics.

	* convert.c (convert_to_integer):  Improve error diagnostics.

2011-04-24  Gary Funck  <gary@intrepid.com>

	* c-parser.c (c_parser_upc_sync_statement): Fix ICE that
	  occurred if there is an error in the barrier id
	  expression.  Map error_mark_node into NULL.

2011-04-22  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172873 into gupc branch.

2011-04-19  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172359 into gupc branch.

2011-04-14  Gary Funck  <gary@intrepid.com>

	* c-family/c.opt: Add UPC as a valid language for various
	  switches where it is valid for "C", that were not updated
	  in previous merges with trunk.

2011-04-13  Gary Funck  <gary@intrepid.com>

	* c-family/c.opt (fupc-pre-include): New option definition.
	* gcc.c (upc_options): Do not add "-include gcc-upc.h" if
	  -fno-upc-pre-include is asserted.

2011-04-07  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172158 into gupc branch.
	to bring in the following fix.

2011-04-05  Bernd Schmidt  <bernds@codesourcery.com>

	PR bootstrap/48403
	* haifa-sched.c (schedule_block): Increment cycle_issued_insns only
	if old and new states differ.

2011-04-06  Gary Funck  <gary@intrepid.com>

	Merge trunk version 171966 into gupc branch.

2011-04-01  Gary Funck  <gary@intrepid.com>

	* tree.h (tree_base): Move UPC flag bits into bit fields
	  located just before spare bits.  Decrement spare bits.
	  Rename 'shared_flag' -> 'upc_shared_flag',
	  'relaxed_flag' -> 'upc_relaxed_flag',
	  'strict_flag' -> 'upc_strict_flag, and adjust macros
	  that reference them accordingly.

2011-04-01  Gary Funck  <gary@intrepid.com>

	* c-decl.c (grokdeclarator): Fix formatting of
	  code that sets UPC block size on scalars.

2011-04-01  Gary Funck  <gary@intrepid.com>

	* c-decl.c (grokdeclarator): Revert use of white space
	  to match the trunk (to rationalize diffs).
	* c-typeck.c (convert_for_assignment): Ditto.
	* tree-ssa.c (useless_type_conversion_p): Ditto.

2011-03-23  Gary Funck  <gary@intrepid.com>

	* DEV-PHASE: bump to 4.7.0-1 to reflect recent creation of
	  the GCC 4.6 release branch.

2011-03-21  Gary Funck  <gary@intrepid.com>

	Merge trunk version 171202 into gupc branch.

2011-03-20  Gary Funck  <gary@intrepid.com>

	* varasm.c: Call error() directly with a format specifier,
	  rather than using sprintf() to format the message.
	  This should make it easier to internationalize UPC's error messages.

2011-03-20  Gary Funck  <gary@intrepid.com>

	* configure.ac: Fix the check for gnu ld when enabling
	  UPC link script support.
	  configure: Regenerate (also picks up changes from previous
	  merge with trunk).

2011-03-20  Gary Funck  <gary@intrepid.com>

	Move UPC start files, end files, and linker specs.
	into libupc.  This reduces the impact on common GCC
	configuration files, and ensures that these UPC-specific
	components are only built when the UPC language dialect is selected.


gcc/
	* c-family/c.opt: Add -fupc-link switch, used to select UPC-specific
	  linker specs. Fix typo in -fupc-debug message.

	* config/upc-conf.h, config/darwin.h: Move defines for
	  UPC-related section begins/ends into libupc/config/default/
	  upc-crt-config.h.

	* config/darwin.h(LINK_COMMAND_SPEC_A): Add call outs to
	  UPC-related linker compiler specifications,
	  accessed via %:include().

	* configure.ac, configure: Remove logic related to building
	  upc-crtbegin/end. Remove config. tests for numa and cpu
	  affinity (previously used by the 'upc' driver); these
	  settings are now propagated by target-specific compiler
	  specs. built by libupc.  Regenerate autoconf.

	* gcc.c (LINK_COMMAND_SPEC): Add call outs to UPC-related
	  linker compiler specifications, accessed via %:include().
	  Define 'upc_crtbegin_spec', 'upc_crtend_spec', and
	  'link_upc_spec'.

	* Makefile.in: Remove definition of UPC_CRTSTUFF_CFLAGS.

	* config/linux.h, config/i386/darwin.h, config/i386/linux64.h,
	  config/i386/linux.h, config/i386/t-darwin, config/i386/t-darwin64,
	  config/ia64/linux.h, config/mips/t-iris,
	  config/mips/iris6.h: Revert to trunk version 167307.

libgcc/
	* configure, configure.ac, config.host, Makefile.in:
	  Revert to trunk version 167307.

2011-02-23  Gary Funck  <gary@intrepid.com>

	* c-decl.c (undeclared_variable): fix typo. Inadvertently
	removed negation on following 'if'.

2011-02-22  Gary Funck  <gary@intrepid.com>

	* c-decl.c (undeclared_variable): call upc_diagnose_deprecated_stmt
	to check for the usage of certain deprecated UPC keywords.
	* c-family/stub-upc.c (upc_diagnose_deprecated_stmt): New.
	* c-tree.h (undeclared_variable): Define prototype.

2011-02-12  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: (build_c_cast, convert_for_assignment)
	  Diagnose an attempt to convert from an integer to
	  a pointer-to-shared as an error.  Also, fix various
	  error messages so that they use the preferred term
	  pointer-to-shared instead of "shared pointer".

2011-02-07  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: (convert_for_assignment)
	  Fix typo. in error message.

2011-01-23  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: (c_build_qualified_type)
	  derive UPC block size by calling upc_get_block_size(),
	  to ensure that the element type of an array of an array
	  is derived correctly.

2010-12-29  Gary Funck  <gary@intrepid.com>

	Merge trunk version 168314 into gupc branch.

2010-12-15  Gary Funck  <gary@intrepid.com>

	Merge trunk version 167307 into gupc branch.

2010-10-19  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: Fix typo in previous fix
	  which led to a mis-compare for equal block sizes.

2010-10-18  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: Fix bug: Cast of (shared <type> * shared) not
	  diagnosed as an error

	  The conversion from any type (shared or not) to
	  a shared type is likely either meaningless or an error.  This update
	  makes any conversion to a shared type, an error.

2010-10-18  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: Fix bug: passing int to shared pointer arg.
	  generates spurious warning

	  Add a #define procedure that does the same thing as
	  WARN_FOR_ASSIGNMENT but issues an error diagnostic instead.  Use
	  this procedure to diagnose passing an integer value to a
	  pointer-to-shared as an error.

2010-10-18  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: Fix bug: shared [] in prototype silently ignored when
	  matching routine declaration.

	  When checking for type compatibility, shared qualified types must
	  have the same block factor.  This check was missing from
	  comptypes_internal().  This update adds the check for blocking
	  factor equality.

2010-10-17  Gary Funck  <gary@intrepid.com>

	* dwarf2out.c: Fix GCC Bug 45870 - note: non-delegitimized UNSPEC 5
	  found (-O1 -g)

	  See: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=45870#c6

2010-10-17  Gary Funck  <gary@intrepid.com>

	* tree-cfg.c: Implement the fix for GCC Bugzilla Bug
	  45869 - [4.5/4.6 Regression] type mismatch in shift expression
	  produces ice with -O3 and -m32.

	  See: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=45869

2010-10-17  Gary Funck  <gary@intrepid.com>

	* c-common.c: Diagnose the application of the various "*sizeof"
	  operations on generic pointer-to-shared as a compilation error.

2010-10-17  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix a segfault/ICE that occurred when printing an error
	  message regarding a function parameter being declared with a shared
	  qualifier.

	  The parameter's 'name' value is not defined at this point, and
	  cannot be used in the error message.  This update removes the
	  reference to 'name', and eliminates the segfault.

2010-10-16  Gary Funck  <gary@intrepid.com>

	* Makefile.in: Implement fixes for SGI/IRIX/MIPS port.

	  The gcc/Makefile.in rules for install-plugin had to be re-written to
	  break up a long list of header files that exceeded the command line
	  limitation imposed by Irix.

	  Access functions for TFmode types had to be implemented.
	  Apparently, this is the mode used for the SGI/MIPS port to represent
	  "long float".

2010-10-14  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix bug: Multiple equal blocking factors specified
	  via typedef chain should not be diagnosed as an error.

	  If the block size that is given by the typedef is equal to the block
	  size given explicitly in the variable declaration, then do not
	  complain.  The easiest way to make this check was to create a
	  temporary type that is a clone of the element type and then set its
	  block size using the given layout qualifier.  Then compare the block
	  size of the temporary (the declaration) to the block size specified
	  in the typedef.  This complexity is needed, because the '[*]' block
	  size needs to be calculated, and the '[]' needs to be mapped into a
	  zero block size.

2010-10-10  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix bug: ICE: two or more layout qualifiers
	  specified

	  The compiler properly detected the presence of two or more layout
	  qualifiers as an error, but then hit an assertion check, because the
	  code that followed the error expected to see a qualifier and not a
	  layout specifier.  The fix is simple: just return immediately after
	  detecting the error.

2010-10-10  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Improve error diagnostics for various cases of UPC
	  shared array type declarations.

	  Add the check for this error: "In the dynamic translation
	  environment, THREADS may not appear in declarations of shared arrays
	  with indefinite block size".  Also, fix up a few of the other
	  related error diagnostics.

2010-10-09  Gary Funck  <gary@intrepid.com>

	* c-common.c: Fix bug: segfault on incomplete array definition.

	  This turned out to be a bit complicated.   The logic in
	  upc_lang_layout_decl had to be re-arranged to avoid trying to lookup
	  the THREADS identifier in the case where the blocking factor has
	  been set to indefinite ([]).  This can happen when indefinite array
	  declarations are processed for shared arrays.  At that time, the
	  file scope has been closed and THREADS is no longer in scope.  Some
	  more work is needed on upc_lang_layout_decl().  It has some
	  duplication, and notably duplicates the two branches of the if
	  having to do with TYPE_SIZE and TYPE_SIZE_UNIT, that appear in the
	  caller of this routine (layout_decl()).

	  The method of forcing a layout qualifier of [] in the indefinite
	  declaration handler is odd a well.  The code that just does the
	  setting of the block factor, needs to moved into its own routine
	  that doesn't depend upon a declspec for '[]' to be passed in, just
	  in order to set the blocking factor to some value (in this case, 0).
	  Also, the logic for how that shared type is constructed is strange.
	  First the type with 0 blocking factor is set.  Then the shared
	  qualifier is removed from the type, and then added back later.  The
	  intermediate type has a blocking factor set, but it has no shared
	  qualifier.  Fixing this will require some thought.  It is tempting
	  just to make indefinite shared arrays an error, rather than forcing
	  the dimension to be '1'.

	  This likely fixes a serious error in the previous update to
	  upc_lang_layout_decl(), where it didn't have the logic to set
	  TYPE_SIZE_UNIT() in the main part of the 'if' statement.  This means
	  the previous update would fail on many tests.

2010-10-09  Gary Funck  <gary@intrepid.com>

	* varasm.c: Add an assertion check for the case that DECL_SIZE_UNIT
	  (decl) is null, which can happen if some layout error occurred
	  upstream.

2010-10-09  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix bug: file scope shared arrays mis-diagnosed as
	  "variable-sized" when compiled in dynamic threads.

	  This long-standing bug is easily fixed.  Just check
	  for the situation that the non-constant sized type is shared and
	  that it does not have a dimension that references a multiple of
	  threads.  If this criteria is met, then issue a meaningful
	  diagnostic.

2010-10-01  Gary Funck  <gary@intrepid.com>

	* configure, configure.ac: Update manual page, and bug reporting
	  URL.

	  Update "man" page to reflect debugging switches.  Also, some general
	  clean up.  Change the bug reporting URL to point to gccupc.org.

2010-09-27  Gary Funck  <gary@intrepid.com>

	* c-parser.c, config/upc-conf.h: Issue a descriptive message when
	  the UPC forall depth count variable is not found.

	  The __upc_forall_depth variable should be defined in gcc-upc-lib.h.
	  The compiler will generate code that references this variable in
	  order to implement nested upc_forall semantics.  If there is a
	  compiler build or install problem, this variable may not be found.
	  In this case, terminate with an internal_error().

2010-09-26  Gary Funck  <gary@intrepid.com>

	* c-parser.c: Fix Bug 240: upc_forall with empty clauses
	  mis-diagnosed as syntax error.

	  Fix a failure exhibited by the Berkeley test case,
	  bug873a.upc, which has the code:
	       double d;
	       upc_forall (;;;d) {...} The compiler did not properly handle
	  the empty "condition" clause, and did not recover well when it was
	  determined that the use of a double value, "d" above, was neither a
	  pointer-to-shared nor an integer expression.  The update implements a
	  fix for both issues.

	  See also: gcc/c-parser.c gcc/upc/upc-act.c

2010-09-23  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix Bug 29: Layout qualifier within a typedef is not
	  incorporated into the referencing type.

	  This was semi-fixed a few times before.  This update fixes a few
	  more places where the layout qualifier wasn't being propagated
	  through typedef's properly.  What made this a bit tricky is shown in
	  the example cited in the bug report:

	  typedef shared [5] int A_t; A_t A[5*THREADS];

	  In the typedef, the blocksize is being applied to a shared *scalar*,
	  and the code was applying the blocksize only to arrays.  This update
	  handles shared scalars correctly.

2010-09-22  Gary Funck  <gary@intrepid.com>

	* c-parser.c, c-tree.h, stub-upc.c: Fix Bug 403: Nested
	  upc_forall() semantics are not implemented

	  The checkforall test in the Berkeley harness test suite indicated
	  that GCC/UPC was not properly implementing nested upc_forall
	  semantics.  Nested upc_forall statements (both statically or
	  dynamically nested) must implement their affinity clause as if it
	  were "continue"; thus all steps in the loop must execute without
	  regard for affinity.  To implement these semantics a global depth
	  counter, __upc_forall_depth, is maintained by the generated code
	  that implements upc_forall.

	  See also: gcc/c-parser.c gcc/c-tree.h gcc/stub-upc.c
	  gcc/upc/upc-act.c gcc/upc/upc-act.h libupc/include/gcc-upc-lib.in
	  libupc/include/upc.h libupc/smp/upc_main.c

2010-09-19  Gary Funck  <gary@intrepid.com>

	* c-decl.c: c-decl.c: zero out the layout specifier, after
	  processing an array type.

	  Fix the previous fix, that moved the setting of the type's layout
	  qualifier to the outside of the array type processing loop.  What is
	  missing from the fix is that the layout_qualifier variable needs to
	  be cleared after setting the type's blocksize.

2010-09-19  Gary Funck  <gary@intrepid.com>

	* config/upc-conf.h: Fix Bug 375: error message is off-by-one when
	  given blocksize is greater than UPC_MAX_BLOCKSIZE.

	  The value we were using for UPC_MAX_BLOCK_SIZE was one bigger than
	  the actual maximum block size.  Therefore, the message was correct,
	  but the underlying value that was being checked was wrong.  Change
	  the values so that they agree with the actual implementation-defined
	  limit.

2010-09-11  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix Bug 402: ICE: '[*]' layout factor on
	  multi-dimensional shared array with dynamic threads.

	  This declaration caused an internal compiler error when compiled
	  with dynamic threads:
	      shared [*] int A[THREADS][16]; The bug was discovered when
	  compiling the RTED_UPC test suite.

	  The fix is to process layout qualifiers after the entire array type
	  has been built.  Otherwise, we try to calculate the blocksize on a
	  shared array type that has not had its "size depends upon the value
	  of THREADS" flag set.

	  Also, added a test case.

	  See also: libupc/testsuite/libupc.upc/intrepid/test18.upc

2010-07-11  Gary Funck  <gary@intrepid.com>

	Fix a bug where a statement in c_build_qualified_type()
	in the trunk had been inadvertently deleted.
	This bug showed up in the IA64 port, because jmpbuf's
	on that architecture must be 16 byte aligned, and they were not.
	c-typeck.c (c_build_qualified_type): Revive the deleted line.

2010-07-08  Gary Funck  <gary@intrepid.com>

	Make changes so that other language compilers
	will build and bootstrap.
	* c-family/c-common.c (compiling_upc, flag_upc,
	flag_upc_instrument, flag_upc_instrument_functions,
	use_upc_dwarf2_extensions): Move to stub-upc.c and
	upc-lang.c.
	* c-family/c-common.h (use_upc_dwarf2_extensions,
	compiling_upc): Add extern definitions - needed
	to compile/build other language front ends.
	* c-family/c.opt: Fix the spelling of ObjC.
	It had been incorrectly spelled as Objc.
	* c-family/c.opt: Add UPC for the various switches
	that are valid for both C and ObjC.
	* c-fmaily/c-opts.c: Add CL_UPC to the list of
	supported options switches when compiling assembly
	language.
	* lto/lto-lang.c (use_upc_dwarf2_extensions,
	flag_upc_instrument, flag_upc_instrument_functions):
	Remove these definitions.  They have been moved
	to c-family/stub-upc.c.
	* upc/ChangeLog: Move relevant entries
	to ChangeLog.upc.
	* c-family/stub-upc.c (compiling_upc, flag_upc,
	flag_upc_instrument, flag_upc_instrument_functions,
	use_upc_dwarf2_extensions): Moved from c-common.c
	* upc/upc-lang.c (compiling_upc, flag_upc,
	flag_upc_instrument, flag_upc_instrument_functions,
	use_upc_dwarf2_extensions): Moved from c-common.c
	* objc/objc-act.c (objc_gimplify_expr): Add additional
	parameters to c_gimplify_expr() call, that are used
	by upc's tree rewrites.
	* cp/cp-gimplify.c (cp_gimplify_expr): Likewise.
	* gcc/cp/lex.c (init_reswords): If not compiling
	UPC, then mask off the UPC keywords.
	* cp/Make-lang.in (C_STUB_OBJS): Use this
	make macro to link with c-family/stub-objc.o
	and c-family/stub-upc.o.
	* fortran/Make-lang.in (C_STUB_OBJS): Likewise.
	* java/Make-lang.in (C_STUB_OBJS): Likewise.
	* objc/Make-lang.in: Link with c-family/stub-upc.o
	to avoid undefined references.
	c-parser.c: Fix line indentation.
	* libcpp/include/cpplib.h: Remove CL_UPC from
	the list of language kinds accepted by the
	C preprocessor.  This is not necessary because
	UPC is a derivative of C99, and does not need
	a different language kind.
	* c-fmaily/c-opts.c (c_common_handle_option):
	Call set_std_c99() when processing the
	"--lang upc" switch, instead of setting the
	language kind to CL_UPC.
	* libupc/testsuite/libupc.upc/intrepid/ChangeLog: New.

2010-07-03  Gary Funck  <gary@intrepid.com>

	Merge trunk version 161517 into gupc branch.
	* gcc/stub-upc.c: Move to gcc/c-family/.
	* libupc/testsuite/libupc.upc/intrepid/test19.upc:
	Add 'unused' attributes to avoid "set, but
	not referenced" warnings.
	* libupc/testsuite/libupc.upc/intrepid/test18.upc:
	Likewise.

2010-03-01 Gary Funck  <gary@intrepid.com>

	Create gupc branch from trunk version 157149.
2011-10-04  Gary Funck  <gary@intrepid.com>

	Merge trunk version 179421 into gupc branch.

	* tree.c (build_opaque_vector_type): Add null UPC layout qualifier
	argument to the call to check_qualified_type().

2011-09-22  Gary Funck  <gary@intrepid.com>

	* tree-pretty-print.c (dump_generic_node): Print UPC type qualifier
	information.
	(dump_upc_type_quals): New.

2011-09-15  Nenad Vukicevic <nenad@intrepid.com>

	Add to FLAGS the flags needed to disable inlining of
	UPC run-time access routines.
	* testsuite/lib/target-supports.exp
	(add_options_for_upc_library_calls): New.

2011-09-15  Gary Funck  <gary@intrepid.com>

	Fix ICE involving shared bit field accesses.
	* tree.c (build3_stat): Propagate TEEE_SHARED()
	TREE_STRICT() and TREE_RELAXED() flags, if applicable.

2011-09-13  Gary Funck  <gary@intrepid.com>

	Merge trunk version 178795 into gupc branch.
	Incorporates fix to PR bootstrap/50010 for x86-32.

2011-09-08  Gary Funck  <gary@intrepid.com>

	Merge trunk version 178557 into gupc branch.

2011-09-07  Gary Funck  <gary@intrepid.com>

	Ensure that UPC pointer-to-shared type alignment is
	propagated to the final type.  Revert to long-standing
	alignment policy: twice the size of a "C" pointer.
	* tree.c (build_pointer_type): Propagate the alignment
	of the UPC pointer-to-shared representation type
	into the newly built pointer type.

2011-08-30  Gary Funck  <gary@intrepid.com>

	* tree.h (check_qualified_type): Change 'const_tree'
	argument types back to 'tree' to avoid complaints
	of assignment drops qualifiers for invocations of the
	newly implemented TYPE_BLOCK_FACTOR() macro, which
	invokes hash functions with 'tree' pointer values that
	are not const qualified.
	* tree.c (check_qualified_type, check_aligned_type): Ditto.
	* c-typeck.c (comptypes_internal): Ditto.

2011-08-29  Gary Funck  <gary@intrepid.com>

	Fixes for regressions noted running "make check"
	versus GCC trunk.
	* fold-const.c (fold_unary_loc): Execute UPC-specific
	checks for a cast-of-a-cast, only if one of the
	operands is a UPC pointer-to-shared value.
	* opts.c (print_specific_help):  Fix a comparison
	in an assert that checks that there are enough
	bits reserved for the number of languages defined
	in the .opt files.

2011-08-29  Gary Funck  <gary@intrepid.com>

	Implement a hash table to record UPC block factors.
	* c-family/stub-upc.c (upc_block_factor_insert,
	upc_block_factor_lookup): New dummy stub procedures.
	* c-family/c-common.c (c_sizeof_or_alignof_type):
	Rename UPC_TYPE_HAS_THREADS_FACTOR() to TYPE_HAS_THREADS_FACTOR().
	* tree.c (copy_node_stat): call SET_TYPE_BLOCK_FACTOR()
	to copy (hashed) UPC blocking factor.
	(set_type_quals): Likewise.
	* tree.h: Rename UPC_TYPE_HAS_THREADS_FACTOR() to
	TYPE_HAS_THREADS_FACTOR().
	(type_common.block_factor): Delete.
	(TYPE_HAS_BLOCK_FACTOR_0, TYPE_HAS_BLOCK_FACTOR_X,
	TYPE_HAS_BLOCK_FACTOR, SET_TYPE_BLOCK_FACTOR): New.
	(TYPE_BLOCK_FACTOR): Re-implement, using hash table for
	UPC blocking factors greater than one.
	* dwarf2out.c (modified_type_die): Re-implement
	logic that records UPC blocking factor in the generated
	DWARF2 debugging information.
	(add_subscript_info): Rename UPC_TYPE_HAS_THREADS_FACTOR() to
	TYPE_HAS_THREADS_FACTOR().
	* c-decl.c (finish_decl, grokdeclarator):
	Rename UPC_TYPE_HAS_THREADS_FACTOR() to TYPE_HAS_THREADS_FACTOR().
	* print-tree.c (print_node): Check for TYPE_LANG_FLAG_* flags
	used by UPC, and print UPC-specific information.

2011-08-28  Gary Funck  <gary@intrepid.com>

	Re-work the type machinery to fully support and to unify support
	for the UPC layout qualifier (blocking factor).
	* c-family/stub-upc.c (upc_set_block_factor): Delete.
	  (upc_grok_layout_qualifier): rename from upc_apply_layout_qualifier()
	  and update prototype.
	* c-family/c-common.c (complete_array_type): call newly
	  defined c_build_qualified_type_1() instead of
	  upc_set_block_factor().
	* c-family/c-upc.h (pc_grok_layout_qualifier): rename from
	  upc_apply_layout_qualifier() and update prototype.
	  (upc_set_block_factor): Delete prototype.
	* c-family/c-common.h (c_build_qualified_type): redefine
	  as a pre-processor macro that invokes c_build_qualified_type_1()
	  with a null UPC layout qualifier.
	  (c_build_qualified_type_1): New.  Add layout qualifier argument
	  to old c_build_qualified_type() prototype.
	* tree.c (set_type_quals, check_qualified_type):
	  Add layout qualifier as argument.
	  (check_aligned_type): Add check for UPC block factor equality.
	  (get_qualified_type_1): Rename from get_qualified_type() and
	  add layout qualifier as argument.
	  (build_qualified_type_1): Rename from build_qualified_type() and
	  add layout qualifier as argument.
	* tree.h (check_qualified_type): Add layout qualifier as argument
	  to the prototype.
	  (get_qualified_type): Re-define as a as a pre-processor macro
	  that invokes get_qualified_type_1() with a null
	  UPC layout qualifier.
	  (get_qualified_type_1): Rename from get_qualified_type() and
	  add layout qualifier as argument.
	* cp/tree.c (c_build_qualified_type_1): Rename from
	  c_build_qualified_type() and add (unused) layout qualifier
	  argument.
	* c-decl.c (finish_decl): re-format long error messages.
	  (grokdeclarator): Re-work the logic so that it calls
	  upc_grok_layout_qualifier() to handle the UPC layout
	  qualifier, if present.
	* c-typeck.c (qualify_type, common_pointer_type,
	  build_component_ref): Re-work the logic so that it calls
	  upc_grok_layout_qualifier() to handle the UPC layout qualifier,
	  if present.
	  (c_build_qualified_type_1): Rename from c_build_qualified_type()
	  and add layout qualifier argument.
	* print_tree.c (print_node): Re-format a long line.

2011-08-26  Gary Funck  <gary@intrepid.com>

	Merge trunk version 177949 into gupc branch.
	* c-family/c-common.h (enum rid): Bump the number of type modifiers
	  in the comment.
	* ../libcpp/init.c (struct lang_flags): Adjust the entry for UPC
	  to add new rliterals column value.

2011-08-26  Gary Funck  <gary@intrepid.com>

	* convert.c (convert_to_integer): Fix the check for the
	  difference between two UPC pointers-to-shared values.

2011-08-23  Nenad Vukicevic <nenad@intrepid.com>

	* configure.ac: Fix the name for --with-upc-pts-packed-bits
	  option. Support --with-upc-packed-bits as deprecated. Fix the code to
	  correctly check packed bits.
	* configure: Re-generate.

2011-08-16  Gary Funck  <gary@intrepid.com>

	* c-parser.c (c_parser_upc_sync_statement): issue error diagnostic if
	  the barrier id expression is not an integer expression.

2011-08-12  Gary Funck  <gary@intrepid.com>

	* config/i386/i386.h (MAX_FIXED_MODE_SIZE): Delete
	  (revert to trunk).
	* upc/upc-pts-struct.c (upc_pts_struct_init_type):
	  Set mode of UPC pointer-to-shared rep. to an
	  integral mode of size at least as large as the
	  size of the representation's struct type.

2011-08-12  Gary Funck  <gary@intrepid.com>

	Rework/simplify the UPC genericize pass.
	* c-family/stub-upc.c (upc_apply_layout_qualifier,
	  upc_build_pointer_type): New.
	  (upc_set_block_factor): Adjust to new calling sequence.
	  (upc_build_shared_var_addr): Delete.
	* c-family/c-common.c (complete_array_type): Adjust call
	  to upc_set_block_factor().
	* c-family/c-upc.h (upc_apply_layout_qualifier,
	  upc_build_pointer_type): New.
	  (upc_build_shared_var_addr): Delete.
	  (upc_set_block_factor): Adjust to new calling sequence.
	* tree.h (TI_UPC_CHAR_PTS_TYPE, upc_char_pts_type_node): New.
	* c-decl.c (grokdeclarator): Call newly defined
	  upc_apply_layout_qualifier() instead of upc_set_block_factor().
	* c-decl.c (grokdeclarator): Split long UPC-related error
	  messages into two lines.
	* c-typeck.c (build_unary_op): Do not call upc_build_shared_var_addr()
	  directly.  Revert that change to trunk. (upc_genericize() will
	  handle lowering the expressions that take the address of a
	  UPC variable).
	* config/i386/i386.c (ix86_promote_function_mode): For UPC
	  pointers-to-shared, return the type mode of the UPC pointer-to-shared
	  representation type.

2011-08-10  Gary Funck  <gary@intrepid.com>

	Implement additional fixes for recent merge with trunk.
	* explow.c (promote_mode): Do not attempt to promote
	  the mode for UPC pointer-to-shared types.
	* config/i386/i386.c (function_value_64): Ditto.

2011-08-07  Gary Funck  <gary@intrepid.com>

	Merge trunk version 177548 into gupc branch.
	* config/i386/i386.c (ix86_promote_function_mode):
	  Do not promote UPC pointers-to-shared to Pmode.
	* c-family/c-ommon.c (pointer_int_sum):
	  Also check the pointer operand, rather than just
	  its type, when making the decision to derive
	  the equivalent unshared type.
	* tree.c (build2_stat): If the type of the result
	  passed in as an argument is a UPC shared type,
	  derive the unshared equivalent when calculating
	  the result type of the node.

2011-07-15  Nenad Vukicevic <nenad@intrepid.com>

	* config/darwin.c: Disable var_tracking option
	  on -O0 as it is supposed to run only when optimization
	  is applied. See GCC bug 49743.

2011-07-06  Gary Funck  <gary@intrepid.com>

	Re-implement the pass that lowers trees generated by
	the UPC front-end into GENERIC.  Previously this was
	done within the gimplification framework, but this
	required that gimplification be run before inlining
	and various other passes, which did not fit in well
	with the current design of the middle-end passes.
	Now, the lowering is done by a newly defined language
	specific genericize hook.  With this change, we are
	are able to undo some extensions made to the gimplify
	logic, and thus can revert several files to trunk.

	* c-decl.c: Add a check for UPC deprecated names which
	  may appear as undefined function names.
	* libfuncs.h (LTI_upc_barrier, LTI_upc_notify, LTI_upc_wait,
	  LTI_upc_getaddr): Remove definitions of unused UPC
	  library function names.  These were defined back when
	  the libfunc interface was used to call these routines.
	* timevar.def (TV_TREE_UPC_GENERICIZE): New. Track time spent
	  in UPC lowering (genericize) pass.
	* langhooks.h (gimplify_expr): Revert to trunk's definition.
	  (instrument_func): Delete this hook.  Now handled in
	  upc_genericize().
	* tree-pass.h: Revert to trunk.
	* c-typeck.c (build_unary_op): Adjust reference to
	  upc_genericize() in a comment.
	* gimplify.c (create_tmp_var_raw): Delete logic that
	  was converting UPC shared types into unshared types
	  to be used for temporaries.  Now handled in  upc_genericize().
	  Replace with an assertion check.
	  (prepare_gimple_addressable): Revert to trunk's definition.
	  No longer needed as an externally called function.
	  (flag_instrument_functions_exclude_p): Re-define as
	  an externally called function.
	  (gimplify_function_tree): Delete code that implemented
	  call to UPC's language specific function instrumentation hook.
	* gimple.h (flag_instrument_functions_exclude_p): Define as extern.
	  (prepare_gimple_addressable): Delete extern definition;
	  revert to trunk's definition.
	* lnaghooks-def.h (lhd_gimplify_expr): Revert to trunk's definition.
	  (LANG_HOOKS_INSTRUMENT_FUNC): Remove, no longer needed for UPC.
	* c-family/c-gimplify.c: Revert to trunk.
	* cp/cp-gimplify.c: Revert to trunk.
	* cp/cp-tree.h: Revert to trunk.
	* explow.c: Revert to trunk.
	* langhooks.c: Revert to trunk.
	* libfuncs.h: Revert to trunk.
	* objc/objc-act.c: Revert to trunk.

2011-06-30  Gary Funck  <gary@intrepid.com>

	* c-family/c.opt: Bring options definitions up-to-date
	with respect to changes made in the trunk.

2011-06-29  Gary Funck  <gary@intrepid.com>

	Merge trunk version 175584 into gupc branch.

2011-06-13  Gary Funck  <gary@intrepid.com>

libcpp/
	* include/cpplib.h (enum c_lang):
	Move the entry for CLK_UPC so that it follows CLK_STDC1X.
	This keeps all the "C" variants together.
	* init.c (lang_defaults): Add an entry for UPC.

2011-06-09  Gary Funck  <gary@intrepid.com>

	* c-parser.c (upc_localsizeof_type): Fix an ICE when compiling for
	a 32-bit target, the size expression overflowed, the result was
	error_mark_node, and an ICE was triggered on an attempt to evaluate
	C_TYPE_VARIABLE_SIZE() on the error node.
	(upc_blocksizeof_expr, upc_blocksizeof_type,
	upc_elemsizeof_expr, upc_elemsizeof_type, upc_localsizeof_expr):
	Make similar changes to avoid further processing of error_mark_node.

2011-06-07  Gary Funck  <gary@intrepid.com>

	* sel-sched.c (move_op): Incorporate the following
	patch to eliminate ICE at -O3 on IA64/Altix.

2011-06-07  Alexander Monakov  <amonakov@ispras.ru>

	* sel-sched.c (move_op): Use correct type for 'res'.  Verify that
	code_motion_path_driver returned 0 or 1.
	(sel_region_finish): Clear h_d_i_d.

2011-06-02  Gary Funck  <gary@intrepid.com>

	Merge trunk version 174558 into gupc branch.

2011-05-30  Gary Funck  <gary@intrepid.com>

	* configure.ac: Make the default order of the virtual
	  address field in a UPC pointer-to-shared to be "first".
	  This reverts to the previous setting, and is a short term
	  measure to work around a bug found on the IA64,
	  where vaddr=last led to incorrect code generation.
	  Rename UPC_PTS_VADDR_FIRST to HAVE_UPC_PTS_VADDR_FIRST
	  in AC_DEFINE(), to agree with recent changes in files
	  that reference this define.
	  configure, config.in: Regenerate.

2011-05-26  Gary Funck  <gary@intrepid.com>

	* configure.ac: Fix typo when referring to $upc_vaddr_order.
	  configure: Regenerate.

	* ../fixincludes/fixincl.x: Revert to trunk.
	  This file is auto-generated, and should not be merged.

2011-05-18  Gary Funck  <gary@intrepid.com>

	Merge trunk version 173845 into gupc branch.

2011-05-17  Gary Funck  <gary@intrepid.com>

	* c-family/stub-upc.c (upc_rts_forall_depth_var): New.
	  c-family/c-upc.h (upc_rts_forall_depth_var): Define.

	* c-family/c-pragma.c: Remove conditional compilation
	  with HANDLE_PRAGMA_UPC and HANDLE_PRAGMA_PUPC.
	  Test 'compiling_upc' when compiling to determine if
	  the "upc" and "pupc" pragmas should be registered.

	* defaults.h (UPC_SHARED_SECTION_NAME, UPC_SHARED_BEGIN_NAME,
	  UPC_SHARED_END_NAME, UPC_PGM_INFO_SECTION_NAME,
	  UPC_PGM_INFO_BEGIN_NAME, UPC_PGM_INFO_END_NAME,
	  UPC_INIT_SECTION_NAME, UPC_INIT_BEGIN_NAME,
	  UPC_INIT_END_NAME, UPC_INIT_ARRAY_SECTION_NAME,
	  UPC_INIT_ARRAY_BEGIN_NAME, UPC_INIT_ARRAY_END_NAME): New.
	  Move these target-dependent definitions from config/upc-conf.h
	  to here.

	* configure.ac: Improve the logic for UPC-related options.
	  Delete references to pre-processor definitions that have
	  been moved to "upc/upc-pts.h".

	* configure, config.in: Regenerate.

	* Makefile.in (UPC_PTS_REP): Remove definition and revert
	  to trunk.  This substitution variable was used to
	  configure the representation-specific versions of
	  the tree rewrites that operate on UPC pointer-to-shared
	  types and objects.

	* c-parser.c (c_parser_upc_forall_statement): Remove
	  reference to UPC_FORALL_DEPTH_NAME, and call
	  newly defined upc_rts_forall_depth_var() instead.

	* config/upc-config.h: Delete. Various definitions
	  moved to "upc/upc-rts-names.h", "defaults.h", and
	  "upc/upc-pts.h".

	* doc/tm.texi.in (HAVE_UPC_PTS_VADDR_FIRST,
	  HAVE_UPC_PTS_PACKED_REP, HAVE_UPC_PTS_STRUCT_REP,
	  UPC_SHARED_SECTION_NAME, UPC_SHARED_BEGIN_NAME,
	  UPC_SHARED_END_NAME, UPC_PGM_INFO_SECTION_NAME,
	  UPC_PGM_INFO_BEGIN_NAME, UPC_PGM_INFO_END_NAME,
	  UPC_INIT_SECTION_NAME, UPC_INIT_BEGIN_NAME,
	  UPC_INIT_END_NAME, UPC_INIT_ARRAY_SECTION_NAME,
	  UPC_INIT_ARRAY_BEGIN_NAME, UPC_INIT_ARRAY_END_NAME):
	  New.  Document UPC target macros.
	  doc/tm.texi: Regenerate.

2011-05-07  Gary Funck  <gary@intrepid.com>

	* ../configure.ac: Disable build of libupc
	  on non POSIX hosted systems.  Use AS_HELP_STRING
	  to define messages.  Remove 'word-pair' as a
	  possible --with-upc-pts UPC pointer-to-shared
	  representation.

	* ../configure: Regenerate.

	* configure.ac: Use AS_HELP_STRING to define messages.
	  Remove 'word-pair' as a possible --with-upc-pts
	  UPC pointer-to-shared representation.
	  (UPC_MAX_THREADS): Limit the maximum value to 2^31-1.
	  (UPC_MAX_BLOCK_SIZE): Correct the default value.

	* configure: Regenerate.

	* config.in: Regenerate. Delete UPC_PTS_WORD_PAIR_REP
	  definition.

	* ChangeLog.upc: Fix some typos.

2011-05-07  Gary Funck  <gary@intrepid.com>

	* ../maintainer-scripts/gcc_release: Add "upc"
	  as one of the released languages.

2011-05-06  Gary Funck  <gary@intrepid.com>

	Eliminate compilation warnings, by fixing
	#include's and updating function prototypes.

	* c-family/c-cppbuiltin.c: Include c-upc.h.

	* optabs.c (gen_libfunc): Change type of 'suffix'
	  parameter to conform with prototype.

	* cp/cp-gimplify.c (cp_gimplify_expr): Add extra parameters
	  ('gimple_test_f' and 'fallback') used by extended gimplify_expr
	  hook used by UPC.

	* cp/cp-tree.h (cp_gimplify_expr): Add extra parameters to
	  the prototype.

	* objc/objc-act.c (objc_gimplify_expr): Pass extra dummy
	  argument values to cp_gimplify_expr.

	* config.in (HAVE_UPC_AFFINITY_SUPPORT,
	  HAVE_UPC_NUMA_SUPPORT): Regenerate.  Remove
	  pre-processor definitions that are no longer
	  needed to build the 'upc' command (upc-cmd.c)
	  because the linker specs. defined in libupc
	  take care of linking in the needed libraries.

	* c-parser.c (c_parser_upc_forall_statement):
	  initialize affinity_loc to avoid "maybe unused" warning.
	  (c_parser_upc_sync_statement): Remove un-needed
	  'ret' variable.  Cast return value from
	  'upc_build_sync_stmt' to 'void' to avoid
	  compile-time warning.

	* config/upc-conf.h (UPC_MAX_THREADS): Define as an
	  integer constant, not a string.  Range is restricted
	  to maximum positive 32-bit integer (2+ billion) to
	  fit in with the use of 'int' in the front-end's
	  switch handling logic.

2011-05-06  Gary Funck  <gary@intrepid.com>

	Upgrade c-family source files to conform with modularity
	improvements.  Mainly, remove #include of c-tree.h in files
	under c-family, and define a new UPC-specific #include file,
	c-upc.h, and use it.

	* c-family/stub-upc.c: Remove #include of c-tree.h and
	  upc/upc-act.h.  Replace with #include of c-common.h
	  and c-upc.h.
	  (upc_get_unshared_type, upc_pts_cvt_op_p, upc_blocksizeof_expr,
	  upc_blocksizeof_type, upc_elemsizeof_expr, upc_elemsizeof_type,
	  upc_localsizeof_expr, upc_localsizeof_type,
	  upc_shared_type_p): Delete.

	* c-family/c-opts.c: Add #include of c-upc.h

	* c-family/c-common.c: Remove #include of c-tree.h and
	  add #include of c-upc.h.

	* c-family/c-upc.h: New. Define API for UPC-specific functions
	  (mostly implemented in upc/upc-act.c).

	* c-family/c-common.h (upc_cpp_builtins, upc_write_global_declarations):
	  Remove extern definitions.

	* c-family/c-pragma.c: Remove #include of c-tree.h.
	  Add #include of c-upc.h.

	* tree.h (UPC_TYPE_HAS_THREADS_FACTOR): New.  Move from
	  c-tree.h.
	  (upc_shared_type_p, upc_pts_cvt_op_p): New.  Move from
	  upc/upc-act.c, and define as a macro.
	  (expand_affinity_test): Remove unused external definition.
	  (build_upc_unshared_type): Add external definition.
	  (upc_shared_type_p): Remove external definition.

	* c-config-lang.in: Update gtfiles to refer to c-family/c-upc.h.

	* dojump.c: Remove #include of c-tree.h.

	* c-tree.h: Remove definition of UPC_TYPE_HAS_THREADS_FACTOR
	  and move to tree.h.
	  (count_upc_threads_refs, is_multiple_of_upc_threads,
	  set_upc_threads_refs_to_one, c_expr, upc_affinity_test,
	  upc_build_shared_var_addr, upc_build_sync_stmt,
	  upc_check_decl_init, upc_check_decl, upc_decl_init, c_expr,
	  upc_get_block_factor, upc_instrument_forall, upc_is_null_pts_p,
	  c_expr, upc_num_threads, upc_diagnose_deprecated_stmt,
	  upc_pts_cvt_op_p, upc_pts_diff, upc_pts_increment,
	  upc_pts_int_sum, upc_set_block_factor, upc_set_decl_section,
	  permit_pragma_upc, deny_pragma_upc, pragma_upc_permitted_p,
	  set_upc_consistency_mode, get_upc_consistency_mode,
	  push_upc_consistency_mode, pop_upc_consistency_mode,
	  get_upc_pupc_mode):
	  Move external definitions to c-family/c-upc.h.
	  (upc_blocksizeof_type, upc_localsizeof_type,
	  upc_elemsizeof_type): Remove external definitions;
	  these functions were moved to c-parser.c.

	* c-decl.c: Add #include of c-upc.h.

	* c-typeck.c: Add #include of c-upc.h.

	* c-convert.c: Add #include of c-upc.h.

	* ChangeLog.upc: Fix typo.

	* Makefile.in: Add references to c-family/c-upc.h, everywhere
	  there is a reference to c-family/c-objc.h.
	  Remove extraneous reference to upc-act.h.

	* c-parser.c: Add #include of c-upc.h.
	  (upc_blocksizeof_expr, upc_blocksizeof_type,
	  upc_elemsizeof_expr, upc_elemsizeof_type,
	  upc_localsizeof_expr, upc_localsizeof_type):
	  Move from upc/upc-act.c.

	* tree.c (build_upc_unshared_type): New.
	  Move upc_get_unshared_type from upc/upc-act.c and rename
	  to build_upc_unshared_type.
	  c-family/c-common.c (pointer_int_sum): refer to renamed
	  build_upc_unshared_type function.
	  c-convert.c (convert): Ditto.
	  convert.c (convert_to_pointer): Ditto.
	  c-typeck.c (build_unary_op, build_modify_expr,
	  really_start_incremental_init): Ditto.
	  gimplify.c (create_tmp_var_raw): Ditto.
	  tree.c (build1_stat): Ditto.
	  tree.h (upc_get_unshared_type): Rename to
	  build_upc_unshared_type.

2011-05-06  Gary Funck  <gary@intrepid.com>

	Merge trunk version 173471 into gupc branch.

2011-05-05  Gary Funck  <gary@intrepid.com>

	Make changes that bring the GUPC branch more closely in sync.
	with the GCC trunk.  Revert any fixes that are not UPC-specific.
	Remove gratuitous re-formatting.

	* ../libstdc++-v3/config/os/bionic/ctype_noninline.h: Delete.
	  This file should have been removed in a previous merge
	  with the trunk.
	* ../configure.ac: Remove Cray Catamount/CNL support.
	* ../configure: Regenerate.
	* ../config.sub: Remove Cray Catamount/CNL support.
	* config.gcc: Remove Cray Catamount/CNL support.
	* ../maintainer-scripts/gcc_release: Revert to trunk.
	* c-family/c-common.h: Define parse_optimize_options.  Its definition
	  was missed in a previous merge with the trunk.
	* dwarf2out.c: Revert a fix which removed the 'type_main_variant'
	  procedure.
	* dwarf2out.c: Revert a fix that added a check for VECTOR_TYPE
	  in addition to ARRAY_TYPE.
	* c-typeck.c: Remove an extra newline character.
	* varasm.c: Revert a fix that improved an error message
	  when TLS common data is unimplemented.
	* varasm.c: Revert a gcc_assert that had been added which
	  checked for a null DECL_SIZE_UNIT field.
	* emultls.c: Revert to trunk. Remove possible fix.
	* Makefile.in: Revert extra blank line that is present
	  in the trunk version.
	* Makefile.in: Revert a fix that handled long shell
	  argument lists for plugin headers.
	* config/ia64/ia64.opt: Revert an option setting
	  that increased the default TLS address range.
	* config/ia64/crtbegin.asm: Revert to trunk.
	  Removes an extra newline character.
	* ChangeLog.upc: Spell check.

2011-05-03  Gary Funck  <gary@intrepid.com>

	* c-family/c-common.c: Remove extraneous FIXME/TODO comments.
	  (c_apply_type_quals_to_decl): Ditto.
	* c-family/c-common.h: Ditto.
	* dojump.c: Ditto.
	* c-decl.c (merge_decls): Ditto.
	* c-typeck.c (qualify_type, default_conversion,
	  build_component_ref, build_unary_op,
	  c_build_qualified_type): Ditto.
	* gimplify.c (create_tmp_var_raw): Remove UPC-specific dead code.

2011-04-29  Gary Funck  <gary@intrepid.com>

	* c-decl.c (finish_decl): Improve error diagnostics.
	  (grokdeclarator): Ditto.

	* c-typeck.c (build_c_cast): Improve error diagnostics.
	  (convert_for_assignment): Ditto.
	  (build_binary_op): Ditto.

	* c-parser.c (c_parser_upc_forall_statement):
	  Improve error diagnostics.

	* convert.c (convert_to_integer):  Improve error diagnostics.

2011-04-24  Gary Funck  <gary@intrepid.com>

	* c-parser.c (c_parser_upc_sync_statement): Fix ICE that
	  occurred if there is an error in the barrier id
	  expression.  Map error_mark_node into NULL.

2011-04-22  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172873 into gupc branch.

2011-04-19  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172359 into gupc branch.

2011-04-14  Gary Funck  <gary@intrepid.com>

	* c-family/c.opt: Add UPC as a valid language for various
	  switches where it is valid for "C", that were not updated
	  in previous merges with trunk.

2011-04-13  Gary Funck  <gary@intrepid.com>

	* c-family/c.opt (fupc-pre-include): New option definition.
	* gcc.c (upc_options): Do not add "-include gcc-upc.h" if
	  -fno-upc-pre-include is asserted.

2011-04-07  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172158 into gupc branch.
	to bring in the following fix.

2011-04-05  Bernd Schmidt  <bernds@codesourcery.com>

	PR bootstrap/48403
	* haifa-sched.c (schedule_block): Increment cycle_issued_insns only
	if old and new states differ.

2011-04-06  Gary Funck  <gary@intrepid.com>

	Merge trunk version 171966 into gupc branch.

2011-04-01  Gary Funck  <gary@intrepid.com>

	* tree.h (tree_base): Move UPC flag bits into bit fields
	  located just before spare bits.  Decrement spare bits.
	  Rename 'shared_flag' -> 'upc_shared_flag',
	  'relaxed_flag' -> 'upc_relaxed_flag',
	  'strict_flag' -> 'upc_strict_flag, and adjust macros
	  that reference them accordingly.

2011-04-01  Gary Funck  <gary@intrepid.com>

	* c-decl.c (grokdeclarator): Fix formatting of
	  code that sets UPC block size on scalars.

2011-04-01  Gary Funck  <gary@intrepid.com>

	* c-decl.c (grokdeclarator): Revert use of white space
	  to match the trunk (to rationalize diffs).
	* c-typeck.c (convert_for_assignment): Ditto.
	* tree-ssa.c (useless_type_conversion_p): Ditto.

2011-03-23  Gary Funck  <gary@intrepid.com>

	* DEV-PHASE: bump to 4.7.0-1 to reflect recent creation of
	  the GCC 4.6 release branch.

2011-03-21  Gary Funck  <gary@intrepid.com>

	Merge trunk version 171202 into gupc branch.

2011-03-20  Gary Funck  <gary@intrepid.com>

	* varasm.c: Call error() directly with a format specifier,
	  rather than using sprintf() to format the message.
	  This should make it easier to internationalize UPC's error messages.

2011-03-20  Gary Funck  <gary@intrepid.com>

	* configure.ac: Fix the check for gnu ld when enabling
	  UPC link script support.
	  configure: Regenerate (also picks up changes from previous
	  merge with trunk).

2011-03-20  Gary Funck  <gary@intrepid.com>

	Move UPC start files, end files, and linker specs.
	into libupc.  This reduces the impact on common GCC
	configuration files, and ensures that these UPC-specific
	components are only built when the UPC language dialect is selected.

gcc/
	* c-family/c.opt: Add -fupc-link switch, used to select UPC-specific
	  linker specs. Fix typo in -fupc-debug message.

	* config/upc-conf.h, config/darwin.h: Move defines for
	  UPC-related section begins/ends into libupc/config/default/
	  upc-crt-config.h.

	* config/darwin.h(LINK_COMMAND_SPEC_A): Add call outs to
	  UPC-related linker compiler specifications,
	  accessed via %:include().

	* configure.ac, configure: Remove logic related to building
	  upc-crtbegin/end. Remove config. tests for numa and cpu
	  affinity (previously used by the 'upc' driver); these
	  settings are now propagated by target-specific compiler
	  specs. built by libupc.  Regenerate autoconf.

	* gcc.c (LINK_COMMAND_SPEC): Add call outs to UPC-related
	  linker compiler specifications, accessed via %:include().
	  Define 'upc_crtbegin_spec', 'upc_crtend_spec', and
	  'link_upc_spec'.

	* Makefile.in: Remove definition of UPC_CRTSTUFF_CFLAGS.

	* config/linux.h, config/i386/darwin.h, config/i386/linux64.h,
	  config/i386/linux.h, config/i386/t-darwin, config/i386/t-darwin64,
	  config/ia64/linux.h, config/mips/t-iris,
	  config/mips/iris6.h: Revert to trunk version 167307.

libgcc/
	* configure, configure.ac, config.host, Makefile.in:
	  Revert to trunk version 167307.

2011-02-23  Gary Funck  <gary@intrepid.com>

	* c-decl.c (undeclared_variable): fix typo. Inadvertently
	removed negation on following 'if'.

2011-02-22  Gary Funck  <gary@intrepid.com>

	* c-decl.c (undeclared_variable): call upc_diagnose_deprecated_stmt
	to check for the usage of certain deprecated UPC keywords.
	* c-family/stub-upc.c (upc_diagnose_deprecated_stmt): New.
	* c-tree.h (undeclared_variable): Define prototype.

2011-02-12  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: (build_c_cast, convert_for_assignment)
	  Diagnose an attempt to convert from an integer to
	  a pointer-to-shared as an error.  Also, fix various
	  error messages so that they use the preferred term
	  pointer-to-shared instead of "shared pointer".

2011-02-07  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: (convert_for_assignment)
	  Fix typo. in error message.

2011-01-23  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: (c_build_qualified_type)
	  derive UPC block size by calling upc_get_block_size(),
	  to ensure that the element type of an array of an array
	  is derived correctly.

2010-12-29  Gary Funck  <gary@intrepid.com>

	Merge trunk version 168314 into gupc branch.

2010-12-15  Gary Funck  <gary@intrepid.com>

	Merge trunk version 167307 into gupc branch.

2010-10-19  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: Fix typo in previous fix
	  which led to a mis-compare for equal block sizes.

2010-10-18  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: Fix bug: Cast of (shared <type> * shared) not
	  diagnosed as an error

	  The conversion from any type (shared or not) to
	  a shared type is likely either meaningless or an error.  This update
	  makes any conversion to a shared type, an error.

2010-10-18  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: Fix bug: passing int to shared pointer arg.
	  generates spurious warning

	  Add a #define procedure that does the same thing as
	  WARN_FOR_ASSIGNMENT but issues an error diagnostic instead.  Use
	  this procedure to diagnose passing an integer value to a
	  pointer-to-shared as an error.

2010-10-18  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: Fix bug: shared [] in prototype silently ignored when
	  matching routine declaration.

	  When checking for type compatibility, shared qualified types must
	  have the same block factor.  This check was missing from
	  comptypes_internal().  This update adds the check for blocking
	  factor equality.

2010-10-17  Gary Funck  <gary@intrepid.com>

	* dwarf2out.c: Fix GCC Bug 45870 - note: non-delegitimized UNSPEC 5
	  found (-O1 -g)

	  See: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=45870#c6

2010-10-17  Gary Funck  <gary@intrepid.com>

	* tree-cfg.c: Implement the fix for GCC Bugzilla Bug
	  45869 - [4.5/4.6 Regression] type mismatch in shift expression
	  produces ice with -O3 and -m32.

	  See: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=45869

2010-10-17  Gary Funck  <gary@intrepid.com>

	* c-common.c: Diagnose the application of the various "*sizeof"
	  operations on generic pointer-to-shared as a compilation error.

2010-10-17  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix a segfault/ICE that occurred when printing an error
	  message regarding a function parameter being declared with a shared
	  qualifier.

	  The parameter's 'name' value is not defined at this point, and
	  cannot be used in the error message.  This update removes the
	  reference to 'name', and eliminates the segfault.

2010-10-16  Gary Funck  <gary@intrepid.com>

	* Makefile.in: Implement fixes for SGI/IRIX/MIPS port.

	  The gcc/Makefile.in rules for install-plugin had to be re-written to
	  break up a long list of header files that exceeded the command line
	  limitation imposed by Irix.

	  Access functions for TFmode types had to be implemented.
	  Apparently, this is the mode used for the SGI/MIPS port to represent
	  "long float".

2010-10-14  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix bug: Multiple equal blocking factors specified
	  via typedef chain should not be diagnosed as an error.

	  If the block size that is given by the typedef is equal to the block
	  size given explicitly in the variable declaration, then do not
	  complain.  The easiest way to make this check was to create a
	  temporary type that is a clone of the element type and then set its
	  block size using the given layout qualifier.  Then compare the block
	  size of the temporary (the declaration) to the block size specified
	  in the typedef.  This complexity is needed, because the '[*]' block
	  size needs to be calculated, and the '[]' needs to be mapped into a
	  zero block size.

2010-10-10  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix bug: ICE: two or more layout qualifiers
	  specified

	  The compiler properly detected the presence of two or more layout
	  qualifiers as an error, but then hit an assertion check, because the
	  code that followed the error expected to see a qualifier and not a
	  layout specifier.  The fix is simple: just return immediately after
	  detecting the error.

2010-10-10  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Improve error diagnostics for various cases of UPC
	  shared array type declarations.

	  Add the check for this error: "In the dynamic translation
	  environment, THREADS may not appear in declarations of shared arrays
	  with indefinite block size".  Also, fix up a few of the other
	  related error diagnostics.

2010-10-09  Gary Funck  <gary@intrepid.com>

	* c-common.c: Fix bug: segfault on incomplete array definition.

	  This turned out to be a bit complicated.   The logic in
	  upc_lang_layout_decl had to be re-arranged to avoid trying to lookup
	  the THREADS identifier in the case where the blocking factor has
	  been set to indefinite ([]).  This can happen when indefinite array
	  declarations are processed for shared arrays.  At that time, the
	  file scope has been closed and THREADS is no longer in scope.  Some
	  more work is needed on upc_lang_layout_decl().  It has some
	  duplication, and notably duplicates the two branches of the if
	  having to do with TYPE_SIZE and TYPE_SIZE_UNIT, that appear in the
	  caller of this routine (layout_decl()).

	  The method of forcing a layout qualifier of [] in the indefinite
	  declaration handler is odd a well.  The code that just does the
	  setting of the block factor, needs to moved into its own routine
	  that doesn't depend upon a declspec for '[]' to be passed in, just
	  in order to set the blocking factor to some value (in this case, 0).
	  Also, the logic for how that shared type is constructed is strange.
	  First the type with 0 blocking factor is set.  Then the shared
	  qualifier is removed from the type, and then added back later.  The
	  intermediate type has a blocking factor set, but it has no shared
	  qualifier.  Fixing this will require some thought.  It is tempting
	  just to make indefinite shared arrays an error, rather than forcing
	  the dimension to be '1'.

	  This likely fixes a serious error in the previous update to
	  upc_lang_layout_decl(), where it didn't have the logic to set
	  TYPE_SIZE_UNIT() in the main part of the 'if' statement.  This means
	  the previous update would fail on many tests.

2010-10-09  Gary Funck  <gary@intrepid.com>

	* varasm.c: Add an assertion check for the case that DECL_SIZE_UNIT
	  (decl) is null, which can happen if some layout error occurred
	  upstream.

2010-10-09  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix bug: file scope shared arrays mis-diagnosed as
	  "variable-sized" when compiled in dynamic threads.

	  This long-standing bug is easily fixed.  Just check
	  for the situation that the non-constant sized type is shared and
	  that it does not have a dimension that references a multiple of
	  threads.  If this criteria is met, then issue a meaningful
	  diagnostic.

2010-10-01  Gary Funck  <gary@intrepid.com>

	* configure, configure.ac: Update manual page, and bug reporting
	  URL.

	  Update "man" page to reflect debugging switches.  Also, some general
	  clean up.  Change the bug reporting URL to point to gccupc.org.

2010-09-27  Gary Funck  <gary@intrepid.com>

	* c-parser.c, config/upc-conf.h: Issue a descriptive message when
	  the UPC forall depth count variable is not found.

	  The __upc_forall_depth variable should be defined in gcc-upc-lib.h.
	  The compiler will generate code that references this variable in
	  order to implement nested upc_forall semantics.  If there is a
	  compiler build or install problem, this variable may not be found.
	  In this case, terminate with an internal_error().

2010-09-26  Gary Funck  <gary@intrepid.com>

	* c-parser.c: Fix Bug 240: upc_forall with empty clauses
	  mis-diagnosed as syntax error.

	  Fix a failure exhibited by the Berkeley test case,
	  bug873a.upc, which has the code:
	       double d;
	       upc_forall (;;;d) {...} The compiler did not properly handle
	  the empty "condition" clause, and did not recover well when it was
	  determined that the use of a double value, "d" above, was neither a
	  pointer-to-shared nor an integer expression.  The update implements a
	  fix for both issues.

	  See also: gcc/c-parser.c gcc/upc/upc-act.c

2010-09-23  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix Bug 29: Layout qualifier within a typedef is not
	  incorporated into the referencing type.

	  This was semi-fixed a few times before.  This update fixes a few
	  more places where the layout qualifier wasn't being propagated
	  through typedef's properly.  What made this a bit tricky is shown in
	  the example cited in the bug report:

	  typedef shared [5] int A_t; A_t A[5*THREADS];

	  In the typedef, the blocksize is being applied to a shared *scalar*,
	  and the code was applying the blocksize only to arrays.  This update
	  handles shared scalars correctly.

2010-09-22  Gary Funck  <gary@intrepid.com>

	* c-parser.c, c-tree.h, stub-upc.c: Fix Bug 403: Nested
	  upc_forall() semantics are not implemented

	  The checkforall test in the Berkeley harness test suite indicated
	  that GCC/UPC was not properly implementing nested upc_forall
	  semantics.  Nested upc_forall statements (both statically or
	  dynamically nested) must implement their affinity clause as if it
	  were "continue"; thus all steps in the loop must execute without
	  regard for affinity.  To implement these semantics a global depth
	  counter, __upc_forall_depth, is maintained by the generated code
	  that implements upc_forall.

	  See also: gcc/c-parser.c gcc/c-tree.h gcc/stub-upc.c
	  gcc/upc/upc-act.c gcc/upc/upc-act.h libupc/include/gcc-upc-lib.in
	  libupc/include/upc.h libupc/smp/upc_main.c

2010-09-19  Gary Funck  <gary@intrepid.com>

	* c-decl.c: c-decl.c: zero out the layout specifier, after
	  processing an array type.

	  Fix the previous fix, that moved the setting of the type's layout
	  qualifier to the outside of the array type processing loop.  What is
	  missing from the fix is that the layout_qualifier variable needs to
	  be cleared after setting the type's blocksize.

2010-09-19  Gary Funck  <gary@intrepid.com>

	* config/upc-conf.h: Fix Bug 375: error message is off-by-one when
	  given blocksize is greater than UPC_MAX_BLOCKSIZE.

	  The value we were using for UPC_MAX_BLOCK_SIZE was one bigger than
	  the actual maximum block size.  Therefore, the message was correct,
	  but the underlying value that was being checked was wrong.  Change
	  the values so that they agree with the actual implementation-defined
	  limit.

2010-09-11  Gary Funck  <gary@intrepid.com>

	* c-decl.c: Fix Bug 402: ICE: '[*]' layout factor on
	  multi-dimensional shared array with dynamic threads.

	  This declaration caused an internal compiler error when compiled
	  with dynamic threads:
	      shared [*] int A[THREADS][16]; The bug was discovered when
	  compiling the RTED_UPC test suite.

	  The fix is to process layout qualifiers after the entire array type
	  has been built.  Otherwise, we try to calculate the blocksize on a
	  shared array type that has not had its "size depends upon the value
	  of THREADS" flag set.

	  Also, added a test case.

	  See also: libupc/testsuite/libupc.upc/intrepid/test18.upc

2010-07-11  Gary Funck  <gary@intrepid.com>

	Fix a bug where a statement in c_build_qualified_type()
	in the trunk had been inadvertently deleted.
	This bug showed up in the IA64 port, because jmpbuf's
	on that architecture must be 16 byte aligned, and they were not.
	c-typeck.c (c_build_qualified_type): Revive the deleted line.

2010-07-08  Gary Funck  <gary@intrepid.com>

	Make changes so that other language compilers
	will build and bootstrap.
	* c-family/c-common.c (compiling_upc, flag_upc,
	flag_upc_instrument, flag_upc_instrument_functions,
	use_upc_dwarf2_extensions): Move to stub-upc.c and
	upc-lang.c.
	* c-family/c-common.h (use_upc_dwarf2_extensions,
	compiling_upc): Add extern definitions - needed
	to compile/build other language front ends.
	* c-family/c.opt: Fix the spelling of ObjC.
	It had been incorrectly spelled as Objc.
	* c-family/c.opt: Add UPC for the various switches
	that are valid for both C and ObjC.
	* c-fmaily/c-opts.c: Add CL_UPC to the list of
	supported options switches when compiling assembly
	language.
	* lto/lto-lang.c (use_upc_dwarf2_extensions,
	flag_upc_instrument, flag_upc_instrument_functions):
	Remove these definitions.  They have been moved
	to c-family/stub-upc.c.
	* upc/ChangeLog: Move relevant entries
	to ChangeLog.upc.
	* c-family/stub-upc.c (compiling_upc, flag_upc,
	flag_upc_instrument, flag_upc_instrument_functions,
	use_upc_dwarf2_extensions): Moved from c-common.c
	* upc/upc-lang.c (compiling_upc, flag_upc,
	flag_upc_instrument, flag_upc_instrument_functions,
	use_upc_dwarf2_extensions): Moved from c-common.c
	* objc/objc-act.c (objc_gimplify_expr): Add additional
	parameters to c_gimplify_expr() call, that are used
	by upc's tree rewrites.
	* cp/cp-gimplify.c (cp_gimplify_expr): Likewise.
	* gcc/cp/lex.c (init_reswords): If not compiling
	UPC, then mask off the UPC keywords.
	* cp/Make-lang.in (C_STUB_OBJS): Use this
	make macro to link with c-family/stub-objc.o
	and c-family/stub-upc.o.
	* fortran/Make-lang.in (C_STUB_OBJS): Likewise.
	* java/Make-lang.in (C_STUB_OBJS): Likewise.
	* objc/Make-lang.in: Link with c-family/stub-upc.o
	to avoid undefined references.
	c-parser.c: Fix line indentation.
	* libcpp/include/cpplib.h: Remove CL_UPC from
	the list of language kinds accepted by the
	C preprocessor.  This is not necessary because
	UPC is a derivative of C99, and does not need
	a different language kind.
	* c-fmaily/c-opts.c (c_common_handle_option):
	Call set_std_c99() when processing the
	"--lang upc" switch, instead of setting the
	language kind to CL_UPC.
	* libupc/testsuite/libupc.upc/intrepid/ChangeLog: New.

2010-07-03  Gary Funck  <gary@intrepid.com>

	Merge trunk version 161517 into gupc branch.
	* gcc/stub-upc.c: Move to gcc/c-family/.
	* libupc/testsuite/libupc.upc/intrepid/test19.upc:
	Add 'unused' attributes to avoid "set, but
	not referenced" warnings.
	* libupc/testsuite/libupc.upc/intrepid/test18.upc:
	Likewise.

2010-03-01 Gary Funck  <gary@intrepid.com>

	Create gupc branch from trunk version 157149.
