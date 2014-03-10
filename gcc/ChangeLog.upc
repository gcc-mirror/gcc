2014-03-10 Gary Funck  <gary@intrepid.com>

	Merge trunk version 208447 into gupc branch.

2014-03-03 Gary Funck  <gary@intrepid.com>

	Merge trunk version 208270 into gupc branch.

2014-02-24 Gary Funck  <gary@intrepid.com>

	Merge trunk version 208066 into gupc branch.

2014-02-17 Gary Funck  <gary@intrepid.com>

	Merge trunk version 207818 into gupc branch.

2014-02-10 Gary Funck  <gary@intrepid.com>

	Merge trunk version 207649 into gupc branch.

2014-02-03 Gary Funck  <gary@intrepid.com>

	Merge trunk version 207415 into gupc branch.

2014-01-30 Gary Funck  <gary@intrepid.com>

	Merge trunk version 207297 into gupc branch.

2014-01-28 Gary Funck  <gary@intrepid.com>

	* c/gupcspec.c (match_suffix): Remove use of PARAMS.

2014-01-20 Gary Funck  <gary@intrepid.com>

	Merge trunk version 206847 into gupc branch.

2014-01-20 Gary Funck  <gary@intrepid.com>

	Merge trunk version 206792 into gupc branch.

2014-01-17 Nenad Vukicevic  <nenad@intrepid.com>

	* c-family/c-cppbuiltin.c (upc_cpp_builtins): Remove pre-defines
	for __UPC_CASTABLE__, __UPC_COLLECTIVE__, __UPC_TICK__ and
	__UPC_PUPC__ as they are library and not compiler features.

2014-01-13 Gary Funck  <gary@intrepid.com>

	Merge trunk version 206575 into gupc branch.

2014-01-10 Gary Funck  <gary@intrepid.com>

	Update copyright notices.

2014-01-06 Gary Funck  <gary@intrepid.com>

	Merge trunk version 206354 into gupc branch.

2014-01-04  Gary Funck  <gary@intrepid.com>

	Integrate GUPC into cc1.
	* upc/: Remove directory.  Re-distribute files.
	* c/c-upc-lang.c: Move upc/upc-lang.c here.
	* c/c-upc-lang.h: New.
	* c-family/c-upc.c: Move upc/upc-act.c here.
	* c-family/c-upc-gasp.c: Move upc/upc-gasp.c here.
	* c-family/c-upc-gasp.h: Move upc/upc-gasp.h here.
	* c-family/c-upc.h: Move upc/upc-act.h here.
	* c-family/c-upc-low.c: Move upc/upc-genericize.c here.
	* c-family/c-upc-low.h: Move upc/upc-genericize.h here.
	* c-family/c-upc-pts.h: Split upc/upc-pts.h here.
	* c-family/c-upc-pts-ops.c: Split upc/upc-pts.c here.
	* c-family/c-upc-pts-ops.h: Split upc/upc-pts.h here.
	* c-family/c-upc-pts-packed.c: Move upc/upc-pts-packed.c here.
	* c-family/c-upc-pts-struct.c: Move upc/upc-pts-struct.c here.
	* c-family/c-upc-rts-names.h: Move upc/upc-rts-names.h here.
	* c-family/stub-upc.c: Remove.
	* c/gupcspec.c: Move upc/gupcspec.c here.
	* doc/gupc.texi: Move upc/gupc.texi here.
	* c/c-objc-common.h: Revert to trunk.
	* cp/lex.c: Revert to trunk.
	* cp/Make-lang.in: Revert to trunk.
	* expr.h: Revert to trunk.
	* flags.h: Revert to trunk.
	* fortran/Make-lang.in: Revert to trunk.
	* java/Make-lang.in: Revert to trunk.
	* lto/Make-lang.in: Revert to trunk.
	* objc/Make-lang.in: Revert to trunk.
	* stor-layout.c: Delete custom hook routines, use
	newly defined declaration layout language hooks.
	* stor-layout.h: Revert to trunk.
	* ../configure.ac: Remove check for UPC language dialect.
	* ../configure: Re-generate.
	* Makefile.in (C_COMMON_OBJS): Add UPC-related object files.
	* c/Make-lang.in: Compile UPC-related files.  Build gupc driver.
	* c/c-decl.c: Call c_genericize() directly rather than
	lang_hooks.genericize().  Adjust for flag_upc and name changes.
	* c/c-lang.c (LANG_HOOKS_UPC_TOGGLE_KEYWORDS,
	LANG_HOOKS_UPC_PTS_STRUCT_INIT_TYPE, LANG_HOOKS_UPC_BUILD_INIT_FUNC,
	LANG_HOOKS_LAYOUT_DECL_P, LANG_HOOKS_LAYOUT_DECL):
	Define UPC-specific hooks.
	* c/c-objc-common.c (upc_types_compatible_p): Move to here.
	(c_types_compatible_p): Call upc_types_compatible_p().
	* c/c-parser.c (upc_affinity_test): Move to here. 
	(upc_build_sync_stmt): Move to here.
	* c/c-typeck.c: #include c-family/c-upc-low.h.
	* c/config-lang.in (gtfiles): Add UPC gt files.
	* c-family/c-common.c: Add #include c-upc.h.
	(c_common_get_alias_set): Move UPC-related alias check to here.
	(upc_num_threads): Move to here.
	(c_common_init_ts): Mark UPC-specific statement tree definitions.
	* c-family/c-common.def (UPC_FORALL_STMT, UPC_SYNC_STMT):
	Define UPC-specific statements.
	* c-family/c-common.h (RID_FIRST_UPC_QUAL, RID_LAST_UPC_QUAL,
	RID_FIRST_UPC_KW, RID_LAST_UPC_KW, UPC_IS_KEYWORD): New.
	(clk_upc, clk_upcxx, c_dialect_upc, compiling_upc): Delete.
	(use_upc_dwarf2_extensions, flag_upc): Move to c-family/c.opts.
	(upc_num_threads): Declare prototype.
	(UPC_SYNC_OP, UPC_SYNC_ID, UPC_SYNC_NOTIFY_OP, UPC_SYNC_WAIT_OP,
	UPC_SYNC_BARRIER_OP): Move definitions here.
	* c-family/c-cppbuiltin.c: Remove #include c-upc.h and
	add #include c-upc-pts.h.
	(upc_cpp_builtins): Move to here.  Define as static.
	(c_cpp_builtin): Change call to c_dialect_upc () into
	test of flag_upc.
	* c-family/c-gimplify.c: #include c-upc-low.h.
	(c_common_genericize): Rename c_genericize() to this and make static.
	(c_genericize): Call upc_genericize() if flag_upc is set and
	then call c_common_genericize().
	* c-family/c-opts.c: #include c-upc-low.h and c-upc-pts.h.
	(c_family_lang_mask): Remove CL_UPC.
	(c_common_option_lang_mask): Remove CL_UPC from lang_flags.
	(upc_init_options): Move to here, make it static.
	(c_common_init_options): Add early check for flag_upc, if found
	call upc_init_options().
	(upc_handle_option): Move to here and make static.
	(c_common_handle_option): Check for UPC-related options
	and call upc_handle_option().  Remove references to
	OPT_lang_upc and clk_upc.  Check flag_upc instead of
	calling c_dialect_upc().
	* c-family/c-pragma.c: #include langhooks.h.
	(handle_pragma_upc): Remove warning if flag_upc not set;
	flag_upc now serves the function of compiling_upc().
	Add call to lang_hooks.upc.toggle_keywords() to
	implement enable/disable of UPC keywords.
	(init_pragma): Check flag_upc instead of compiling_upc.
	* c-family/c-pragma.h (deny_pragma_upc, get_upc_consistency_mode,
	permit_pragma_upc, pop_upc_consistency_mode,
	pragma_upc_permitted_p, push_upc_consistency_mode,
	set_upc_consistency_mode): Move prototypes to here.
	* c-family/c.opt: Remove all references to "UPC" language dialect.
	Add -fupc option. Delete -fupc-link option.
	Add -fupc-threads= option and deprecate -fupc-threads-.
	* configure.ac: Remove check for UPC language dialect.
	* configure: Re-generate.
	* explow.c (tree_expr_size): Move to tree.c.
	* gcc.c: Change specs to refer to -fupc instead of -lang-upc
	and -fupc-link.
	* hooks.c (hook_bool_tree_tree_false): Declare prototype.
	* langhooks.c (lhd_do_nothing_b, lhd_do_nothing_t_t): New.
	* langhooks.h (lang_hooks_for_upc): Define hooks for UPC.
	(layout_decl_p, layout_decl): Define language-specific
	declaration  layout hooks.
	(genericize): Remove this language hook.
	* langhooks-def.h: Define UPC default language hooks.
	Define language-specific declaration layout default hooks.
	* langhooks.c (lhd_do_nothing_b): New.
	* tree-core.h (shared_flag): Rename from upc_shared_flag.
	(strict_flag): Rename from upc_strict_flag.
	(relaxed_flag): Rename from upc_relaxed_flag.
	(threads_factor_flag): New. Was a lang. flag.
	(block_factor_0): New. Was a lang. flag.
	(block_factor_x): New. Was a lang. flag.
	(spare1): Decrement spare bits from 8 down to 5.
	* tree.c (block_factor_for_type): Move to here.
	(tree_expr_size): Move to here from explow.c.
	(block_factor_lookup): Move to here.
	(block_factor_insert): Move to here.
	(upc_get_block_factor): Move to here.
	* tree.h: refer to block_factor_* instead of upc_block_factor_*.
	(TYPE_HAS_BLOCK_FACTOR_0, TYPE_HAS_BLOCK_FACTOR_X,
	TYPE_HAS_THREADS_FACTOR): Refer to tree base flag bits instead
	of lang. flag bits.
	(tree_expr_size): Move prototype from explow.h.

2013-12-30 Gary Funck  <gary@intrepid.com>

	Merge trunk version 206243 into gupc branch.

2013-12-23 Gary Funck  <gary@intrepid.com>

	Merge trunk version 206179 into gupc branch.

2013-12-22 Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_cpp_builtins): Bump UPC_VERSION
	to reflect UPC specification version 1.3 compliance.

2013-12-16 Gary Funck  <gary@intrepid.com>

	Merge trunk version 206010 into gupc branch.

2013-12-09 Gary Funck  <gary@intrepid.com>

	Merge trunk version 205801 into gupc branch.

2013-12-03 Meador Inge  <meadori@codesourcery.com>

	* tree-core.h (tree_type_common): Change tree_type_common to use
	user-provided GC marking.
	(gt_ggc_mx, gt_pch_nx): New prototypes.
	* tree.c (gt_ggc_mx, gt_pch_nx): New functions.

2013-12-03 Gary Funck  <gary@intrepid.com>

	Revert:

	2011-10-10  Gary Funck  <gary@intrepid.com>

	* tree.c (check_qualified_type, check_aligned_type):
	Call tree_int_cst_equal() to compare UPC blocking factors
	if the corresponding tree pointers are not equal.
	* c-typeck.c (comptypes_internal, c_build_qualified_type_1): Ditto.

2013-12-03 Gary Funck  <gary@intrepid.com>

	* c-family/c.opt: Fix typo introduced in 2013-06-03
	merge with trunk.

2013-12-02 Gary Funck  <gary@intrepid.com>

	Merge trunk version 205582 into gupc branch.

2013-11-26  Meador Inge  <meadori@codesourcery.com>

	* upc/upc-genericize.c (upc_expand_get): Ensure that temporaries
	are declared via a DECL_EXPR.

2013-11-25 Gary Funck  <gary@intrepid.com>

	Merge trunk version 205346 into gupc branch.

2013-11-18 Gary Funck  <gary@intrepid.com>

	Merge trunk version 204942 into gupc branch.

2013-11-15 Gary Funck  <gary@intrepid.com>

	Merge trunk version 204894 into gupc branch.

2013-11-15 Gary Funck  <gary@intrepid.com>

	Merge trunk version 204659 into gupc branch.

2013-11-04 Gary Funck  <gary@intrepid.com>

	Merge trunk version 204345 into gupc branch.

2013-10-31 Gary Funck  <gary@intrepid.com>

	Released GUPC 4.9.0.1 based on version 203902.
	This release version was committed on 2013-10-21.
	* DEV-PHASE: Bump to 4.9.0.2.
	* DATESTAMP: Bump date stamp.

2013-10-28 Gary Funck  <gary@intrepid.com>

	Merge trunk version 204119 into gupc branch.

2013-10-21 Gary Funck  <gary@intrepid.com>

	Merge trunk version 203886 into gupc branch.

2013-10-14 Gary Funck  <gary@intrepid.com>

	Merge trunk version 203514 into gupc branch.

2013-10-07 Gary Funck  <gary@intrepid.com>

	Merge trunk version 203240 into gupc branch.

2013-10-01 Gary Funck  <gary@intrepid.com>

	Implement pointer-to-shared -> integer conversions.
	Required per UPC 1.3 Specification.
	* c/c-typeck.c (build_c_cast): Remove logic that diagnosed
	PTS->int conversions as an error.  Rewrite into a CONVERT_EXPR
	for later processing by upc_genericize().

2013-10-01 Gary Funck  <gary@intrepid.com>

	Implement pointer-to-shared -> integer conversions.
	Required per UPC 1.3 Specification.
	* upc/upc-genericize.c (upc_genericize_pts_to_int_cvt): New.
	(upc_genericize_expr): Call upc_genericize_pts_to_int_cvt().

2013-09-30 Gary Funck  <gary@intrepid.com>

	Merge trunk version 203026 into gupc branch.

2013-09-23 Gary Funck  <gary@intrepid.com>

	Merge trunk version 202825 into gupc branch.

2013-09-18 Gary Funck  <gary@intrepid.com>

	Merge trunk version 202725 into gupc branch.

2013-09-16 Gary Funck  <gary@intrepid.com>

	Merge trunk version 202619 into gupc branch.

2013-09-09 Gary Funck  <gary@intrepid.com>

	Merge trunk version 202382 into gupc branch.

2013-09-02 Gary Funck  <gary@intrepid.com>

	Merge trunk version 202159 into gupc branch.

2013-08-26 Gary Funck  <gary@intrepid.com>

	Merge trunk version 202008 into gupc branch.

2013-08-19 Gary Funck  <gary@intrepid.com>

	Merge trunk version 201832 into gupc branch.

2013-08-05 Gary Funck  <gary@intrepid.com>

	Merge trunk version 201483 into gupc branch.

2013-07-29 Gary Funck  <gary@intrepid.com>

	Merge trunk version 201301 into gupc branch.

2013-07-22 Gary Funck  <gary@intrepid.com>

	Merge trunk version 201119 into gupc branch.

2013-07-15 Gary Funck  <gary@intrepid.com>

	Merge trunk version 200955 into gupc branch.

2013-07-08 Gary Funck  <gary@intrepid.com>

	Merge trunk version 200775 into gupc branch.

2013-07-01 Gary Funck  <gary@intrepid.com>

	Merge trunk version 200575 into gupc branch.

2013-06-24 Gary Funck  <gary@intrepid.com>

	Merge trunk version 200361 into gupc branch.

2013-06-19 Gary Funck  <gary@intrepid.com>

	Merge trunk version 200149 into gupc branch.

2013-06-19 Gary Funck  <gary@intrepid.com>

	Merge trunk version 200149 into gupc branch.
	* upc/upc-act.c (upc_create_static_var): New.
	(upc_build_init_func): Call upc_create_static_var() to
	create a static variable, __upc_init_func_addr, which
	is initialized to the address of the UPC
	shared data initialization function.  This change is
	needed to avoid writing to the output assembly language
	file too early.
	(upc_build_sync_stmt): Delete unused variable, sync_expr_type.

2013-06-03 Gary Funck  <gary@intrepid.com>

	Merge trunk version 199596 into gupc branch.

2013-05-27 Gary Funck  <gary@intrepid.com>

	Merge trunk version 199350 into gupc branch.

2013-05-20 Gary Funck  <gary@intrepid.com>

	Merge trunk version 199093 into gupc branch.

2013-05-14 Gary Funck  <gary@intrepid.com>

	* upc/upc-pts-packed.c (upc_pts_packed_build_cvt):
	When checking whether the phase of a PTS should be reset,
	if the source type is an array type, then bypass
	the check for equal type sizes.
	* upc/upc-pts-struct.c (upc_pts_struct_build_cvt): Ditto.

2013-05-13 Gary Funck  <gary@intrepid.com>

	Merge trunk version 198815 into gupc branch.

2013-05-06 Gary Funck  <gary@intrepid.com>

	Merge trunk version 198622 into gupc branch.

2013-04-29 Gary Funck  <gary@intrepid.com>

	Merge trunk version 198433 into gupc branch.

2013-04-15 Gary Funck  <gary@intrepid.com>

	Merge trunk version 197958 into gupc branch.

2013-04-08 Gary Funck  <gary@intrepid.com>

	Merge trunk version 197571 into gupc branch.

2013-04-04 Gary Funck  <gary@intrepid.com>

	Per the UPC 1.3 specification, the type of the
	optional barrier/notify/wait expression is not constrained to
	'int'. Instead, any type that is assignment compatible
	with an 'int' type is permitted.
	* c/c-parser.c (c_parser_upc_sync_statement): Do not check
	the optional expression type here.  Let upc_build_sync_stmt()
	handle it.
	* c/c-tree.h (c_cvt_expr_for_assign): Declare prototype.
	* c/c-typeck.c (c_cvt_expr_for_assign): New.  Also, call
	error_at() in lieu of error() when the source location is known.
	* upc/upc-act.c (upc_build_sync_stmt): Call c_cvt_expr_for_assign()
	to check/convert the optional synchronization statement
	expression.

2013-04-02 Gary Funck  <gary@intrepid.com>

	Merge trunk version 197340 into gupc branch.

2013-04-02 Gary Funck  <gary@intrepid.com>

	Revert revision 178346 (2011-08-30)
	which changed the prototype of check_qualified_type and related
	functions so that their parameters were just 'tree' and not
	'const_tree'.  This seemed necessary at the time because of a
	change to TYPE_BLOCK_FACTOR(), which in turn called function
	that hashed the type node pointer.  The hash functions did
	not accept "const void *" pointers.  Implement a work around
	that removes this restriction.
	* c/c-typeck.c: Revert.
	* tree.c: Revert.
	* tree.h: Revert.  Change prototype of upc_block_factor_lookup()
	to accept a 'const_tree' pointer to a type node.
	* c-family/stub-upc.c (upc_block_factor_lookup): Update prototype.
	* upc/upc-act.c (upc_block_factor_lookup): Accept const_tree
	input argument and convert this to 'tree' for use with hash function.

2013-04-02 Gary Funck  <gary@intrepid.com>

	Revert revision 178346 (2011-08-30)
	which changed the prototype of check_qualified_type and related
	functions so that their parameters were just 'tree' and not
	'const_tree'.  This seemed necessary at the time because of a
	change to TYPE_BLOCK_FACTOR(), which in turn called a function
	that hashed the type node pointer.  The hash functions did
	not accept "const void *" pointers.  Implement a work around
	that removes this restriction.
	* upc/upc-act.c (upc_block_factor_lookup): Accept const_tree
	input argument and convert this to 'tree' for use with hash function.

2013-03-25 Gary Funck  <gary@intrepid.com>

	Merge trunk version 197029 into gupc branch.

2013-03-21 Gary Funck  <gary@intrepid.com>

	Released GUPC 4.8.0.3 based on version 196601.
	This release version was committed on 2013-03-11.
	* DATESTAMP: Bump date stamp.

2013-03-18 Gary Funck  <gary@intrepid.com>

	Merge trunk version 196771 into gupc branch.
	* DEV-PHASE: Bump release identifier to 4.9.0-1.

2013-03-11 Gary Funck  <gary@intrepid.com>

	Merge trunk version 196592 into gupc branch.

2013-03-04 Gary Funck  <gary@intrepid.com>

	Merge trunk version 196422 into gupc branch.

2013-02-25 Gary Funck  <gary@intrepid.com>

	Merge trunk version 196253 into gupc branch.

2013-02-18 Gary Funck  <gary@intrepid.com>

	Merge trunk version 196115 into gupc branch.

2013-02-11 Gary Funck  <gary@intrepid.com>

	Merge trunk version 195937 into gupc branch.

2013-02-04 Gary Funck  <gary@intrepid.com>

	Merge trunk version 195707 into gupc branch.

2013-01-28 Gary Funck  <gary@intrepid.com>

	Merge trunk version 195502 into gupc branch.

2013-01-21 Gary Funck  <gary@intrepid.com>

	Merge trunk version 195330 into gupc branch.

2013-01-14 Gary Funck  <gary@intrepid.com>

	Merge trunk version 195164 into gupc branch.

2013-01-07 Gary Funck  <gary@intrepid.com>

	Merge trunk version 194962 into gupc branch.

2012-12-24 Gary Funck  <gary@intrepid.com>

	Merge trunk version 194709 into gupc branch.

2012-12-17 Gary Funck  <gary@intrepid.com>

	Merge trunk version 194552 into gupc branch.

2012-12-10 Gary Funck  <gary@intrepid.com>

	Merge trunk version 194351 into gupc branch.

2012-12-03 Gary Funck  <gary@intrepid.com>

	Merge trunk version 194076 into gupc branch.

2012-11-30  Gary Funck  <gary@intrepid.com>

	Released GUPC 4.8.0.2 based on version 193446.
	Date of release: 2012-11-12.
	* DEV-PHASE: Bump release identifier to 4.8.0-3.
	* DATESTAMP: Bump date stamp.

2012-11-26 Gary Funck  <gary@intrepid.com>

	Merge trunk version 193807 into gupc branch.

2012-11-20 Gary Funck  <gary@intrepid.com>

	Merge trunk version 193672 into gupc branch.

2012-11-19 Gary Funck  <gary@intrepid.com>

	Merge trunk version 193617 into gupc branch.

2012-11-15 Gary Funck  <gary@intrepid.com>

	Merge trunk version 193426 into gupc branch.

2012-11-12  Gary Funck  <gary@intrepid.com>

	Released GUPC 4.8.0.1 based on version 192948.
	Date of release: 2012-10-29.
	* DEV-PHASE: Bump release identifier to 4.8.0-2.
	* DATESTAMP: Bump date stamp.

2012-11-05 Gary Funck  <gary@intrepid.com>

	Merge trunk version 193152 into gupc branch.

2012-10-29 Gary Funck  <gary@intrepid.com>

	Merge trunk version 192909 into gupc branch.

2012-10-28  Nenad Vukicevic  <nenad@intrepid.com>

	Remove unused --upc-pthreads-per-process compile switch.
	* c-family/c.opt: Remove --upc-pthreads-per-process switch
	that compiler/runtime do not use.
	* c-family/c-opts.c (c_common_handle_option): Remove support for
	--upc-pthreads-per-process switch.
	* c-family/c-common.c: Ditto.
	* c-family/c-common.h: Ditto.

2012-10-28  Nenad Vukicevic  <nenad@intrepid.com>

	Remove unused --upc-pthreads-per-process compile switch. 
	* upc/upc-act.c (upc_handle_option): Ditto.
	(upc_cpp_builtins): Ditto.
	* upc/upc-lang.c (upc_init_options): Ditto.

2012-10-27 Gary Funck  <gary@intrepid.com>

	* defaults.h: fix typos and formatting in UPC-related entries.

2012-10-26  Nenad Vukicevic  <nenad@intrepid.com>

	Place shared initialization code into the .text
	section instead of a separate .upc_init section.
	* defaults.h (UPC_INIT_SECTION_NAME): Delete.
	(UPC_INIT_BEGIN_NAME): Delete.
	(UPC_INIT_END_NAME): Delete.
	* doc/tm.texi.in: Ditto.
	* doc/tm.texi: Ditto.

2012-10-26  Nenad Vukicevic  <nenad@intrepid.com>

	Place shared initialization code into the .text
	section instead of a separate .upc_init section.
	* upc/upc-act.c (upc_build_init_func): Remove settings
	of the section for shared initialization code.

2012-10-22 Gary Funck  <gary@intrepid.com>

	Merge trunk version 192673 into gupc branch.

2012-10-15 Gary Funck  <gary@intrepid.com>

	Merge trunk version 192449 into gupc branch.

2012-10-08 Gary Funck  <gary@intrepid.com>

	Merge trunk version 192198 into gupc branch.

2012-10-01 Gary Funck  <gary@intrepid.com>

	Merge trunk version 191931 into gupc branch.

2012-09-26  Gary Funck  <gary@intrepid.com>

	Implement support for various UPC version 1.3 specification
	additions and changes. Deprecate support for upc_local_alloc.
	Add support for upc_tick (wall-clock timer) library.
	Add support for collective de-allocation functions:
	upc_all_free and upc_all_lock_free.
	Consult libgupc/ChangeLog,
	libgupc/testsuite/libgupc.upc/intrepid/ChangeLog for details.

2012-09-26  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_cpp_builtins): Pre-define __UPC_TICK__.

2012-09-24 Gary Funck  <gary@intrepid.com>

	Merge trunk version 191658 into gupc branch.

2012-09-17  Gary Funck  <gary@intrepid.com>

	Merge trunk version 191376 into gupc branch.

2012-09-10  Gary Funck  <gary@intrepid.com>

	Merge trunk version 191141 into gupc branch.

2012-08-29  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/gupc.texi: Add description for -fupc-pre-include.

2012-08-27  Gary Funck  <gary@intrepid.com>

	Merge trunk version 190707 into gupc branch.
	* tree.h (struct tree_base): Adjust various
	UPC tree and type flags to refer to the
	newly introduced u.bits field.

2012-08-20  Gary Funck  <gary@intrepid.com>

	Merge trunk version 190524 into gupc branch.
	Incorporates a fix in genoutput.c (to properly use
	CONST_CAST) that led to build failures with
	older versions of g++.

2012-08-16  Gary Funck  <gary@intrepid.com>

	Merge trunk version 190437 into gupc branch.
	Incorporates a fix for a build failure on the PPC
	due to passing incorrect switches to the assembler.
	Also, first merged trunk revision that compiles GCC
	with the C++ compiler in the first stage.

2012-08-16  Gary Funck  <gary@intrepid.com>

	* upc/upc-pts-struct.c (upc_pts_struct_is_null_p):
	Adjust VEC_index() calls to use C++ syntax.
	This is required as part of the move to compile
	GCC with the C++ compiler.

2012-08-13  Gary Funck  <gary@intrepid.com>

	Merge trunk version 190336 into gupc branch.

2012-08-09  Gary Funck  <gary@intrepid.com>

	* c-family/c-common.c (c_fully_fold_internal): Do not fold
	offsetof-like expressions when they are applied to UPC
	shared types.

2012-08-07  Gary Funck  <gary@intrepid.com>

	* upc/upc-genericize.c: Delete un-used include of optabs.h.
	Fixes a parallel make failure due to un-noticed dependency.

2012-08-06  Gary Funck  <gary@intrepid.com>

	Merge trunk version 190173 into gupc branch.

2012-08-01  Gary Funck  <gary@intrepid.com>

	Merge trunk version 190063 into gupc branch.
	Incorporates fix for build failure on IA64.

2012-07-30  Gary Funck  <gary@intrepid.com>

	Merge trunk version 189954 into gupc branch.

2012-07-27  Gary Funck  <gary@intrepid.com>

	Merge trunk version 189856 into gupc branch.
	Incorporates fix for bootstrap with a sub-set of language
	specific source directories present.
	* optabs.c: Revert to trunk.  Rather than defining opcodes
	as a way of defining get/put runtime library function names,
	generate them directly in gcc/upc/upc-genericize.c.
	* optabs.h: Ditto.
	* genopinit.c: Ditto.

2012-07-27  Gary Funck  <gary@intrepid.com>

	* upc/upc-genericize.c (upc_expand_get, upc_expand_put):
	Rather than referring to UPC-specific opcodes to find
	the name of the get/put library function name, generate
	the name directly.
	(get_lc_mode_name): New.

2012-07-25  Gary Funck  <gary@intrepid.com>

	* upc/upc-pts-struct.c (upc_pts_struct_build_cond_expr):
	Fix regression: field-by-field comparison of UPC
	pointer-to-shared (vaddr, thread) only works
	(with the current logic) for the == and != operators.

2012-07-24  Gary Funck  <gary@intrepid.com>

	* upc/upc-pts-packed.c (upc_pts_packed_build_cond_expr):
	Fix warning about use of const_tree.

2012-07-24  Gary Funck  <gary@intrepid.com>

	Per UPC spec. 6.4.2p6, ignore the value of the
	phase of a pointer-to-shared, when comparing for
	equal or not equal.
	* upc/upc-pts-packed.c (upc_pts_packed_build_cond_expr):
	Use bit-wise comparison only if the UPC pointer-to-shared
	target type has a block size <= 1 and the representation
	has vaddr first or the comparison is for equality/inequality.
	* upc/upc-pts-struct.c (upc_pts_struct_build_cond_expr):
	Use (vaddr, thread) comparison only if the UPC pointer-to-shared
	has a block size <= 1 or the comparison is for equality/inequality.

2012-07-23  Gary Funck  <gary@intrepid.com>

	Merge trunk version 189777 into gupc branch.

2012-07-16  Gary Funck  <gary@intrepid.com>

	Merge trunk version 189545 into gupc branch.

2012-07-12  Gary Funck  <gary@intrepid.com>

	Merge trunk version 189366 into gupc branch.

2012-07-05  Gary Funck  <gary@intrepid.com>

	Merge trunk version 189274 into gupc branch.
	Incorporates graphite build infrastructure changes.

2012-07-04  Gary Funck  <gary@intrepid.com>

	Merge trunk version 189080 into gupc branch.
	* Makefile.in: Adjust for move of C front-end files.
	* c/Make-lang.in: Ditto.

2012-07-04  Gary Funck  <gary@intrepid.com>

	* upc/config-lang.in: Adjust for move of C front-end files.
	* upc/upc-act.c: Look for c-tree.h and c-objc-common.h in c/.
	* upc/upc-gasp.c: Ditto.
	* upc/upc-genericize.c: Ditto.
	* upc/upc-lang.c: Ditto.
	* upc/upc-pts-struct.c: Ditto.

2012-07-03  Gary Funck  <gary@intrepid.com>

	Merge trunk version 189078 into gupc branch.

2012-06-25  Gary Funck  <gary@intrepid.com>

	Merge trunk version 188931 into gupc branch.

2012-06-18  Gary Funck  <gary@intrepid.com>

	Merge trunk version 188721 into gupc branch.

2012-06-11  Gary Funck  <gary@intrepid.com>

	Merge trunk version 188380 into gupc branch.

2012-06-04  Gary Funck  <gary@intrepid.com>

	Merge trunk version 188168 into gupc branch.

2012-05-29  Gary Funck  <gary@intrepid.com>

	Merge trunk version 187927 into gupc branch.

2012-05-19  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_lang_layout_decl): Ignore declarations of
	an array of shared type declarations if the size of the
	array is zero.  This avoids a segfault when processing
	the UPC blocking factor.

2012-05-18  Gary Funck  <gary@intrepid.com>

	Merge trunk version 187666 into gupc branch.
	Incorporates fix for ICE in tree vectorization pass
	when processing strided loads.

2012-05-17  Gary Funck  <gary@intrepid.com>

	Merge trunk version 187578 into gupc branch.

2012-05-09  Gary Funck  <gary@intrepid.com>

	Merge trunk version 187347 into gupc branch.
	Incorporates fix for segfault in tree vectorization pass.

2012-05-08  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/gupcspec.c (lang_specific_driver): Improve handling of
	"-x" switches: (a) add "-x upc" switch for C source files if
	no -x switch was seen before OR "-x none" was seen, (b) add
	"-x none" for files other then C files if "-x upc" was
	previously added. Cleanup warnings on unused variables
	and integer conversion. Print verbose info regardless of the
	command line being the same.

2012-05-04  Nenad Vukicevic  <nenad@intrepid.com>

	* testsuite/lib/upc.exp: Use gupc instead of xgupc driver.
	Appropriate libraries and include files are added on the
	command line to make it possible to compile with the driver
	from the build tree.

2012-05-04  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/gupc.texi: Simplify the description of the optimization
	options.

2012-05-04  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/Make-lang.in: Remove build of xgupc.
	* upc/gupcspec.c (get_libgupc_path): Delete.
	(lang_specific_driver): Remove support for building xgupc.
	Removed code tried to add -B, -L, -isystem to the command
	line if xgupc driver is invoked from the development tree.

2012-05-04  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_num_threads): When THREADS is specified
	statically, convert the value to a signed size type,
	so that (for example) the thread affinity test in a
	upc_forall() statement will work as expected for negative
	integer index values.

2012-04-30  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/Make-lang.in: Add year 2012 to the copyright.

2012-04-30  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/gupc.c: Delete. The old GUPC driver.
	* upc/gupcspec.c: Add. The new GUPC driver tailored
	after fortran/gfortranspec.c driver. Options "-n", "-inst",
	and "-inst-functions' are not supported by the new driver.
	Support for building gupc and xgupc remains the same.
	* upc/Make-lang.in (xgupc): Changes to compile gupcspec.c
	instead of gupc.c
	(gupc): Ditto.
	* upc/gupc.texi: Remove '-n', '-inst', and '-inst-functions'
	options.

2012-04-29  Nenad Vukicevic  <nenad@intrepid.com>

	* config/rs6000/rs6000.c (rs6000_return_in_memory): Conform to
	PPC ABI. In the UPC 'struct' pointer-to-shared representation,
	a function returns a pointer-to-shared in memory instead
	of in registers.
	(rs6000_pass_by_reference): Ditto.

2012-04-28  Gary Funck  <gary@intrepid.com>

	* c-decl.c (grokdeclarator): Set 'type' to error node
	after detecting "shared auto variable" error to avoid
	downstream complications.
	* upc/upc-act.c: Fix a couple of spelling errors in comments.

2012-04-26  Gary Funck  <gary@intrepid.com>

	* c-family/c-pragma.c (handle_pragma_upc):
	Fix typo in warning message.

2012-04-17  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/gupc.texi: Fix the warning for usage of 'insertcopying'
	before 'copying'. Minor changes to to copyright years and
	manual info.

2012-04-17  Gary Funck  <gary@intrepid.com>

	Merge trunk version 186486 into gupc branch.

2012-04-17  Gary Funck  <gary@intrepid.com>

	* c-family/c-pragma.c (disable_pupc_mode): Return the previous
	value of the pupc mode.  Fixes a compile-time warning.
	* c-family/c-pragma.c (init_pragma_pupc, get_upc_pupc_mode,
	disable_pupc_mode, set_pupc_mode, handle_pragma_pupc):
	Improve source formatting.

2012-04-16  Gary Funck  <gary@intrepid.com>

	* upc/upc-genericize.c: Adjust copyright.
	(upc_genericize_fndecl): Adjust call graph
	union member reference to use the newly introduced
	'symbol' field in order to refer to 'decl'.

2012-04-09  Gary Funck  <gary@intrepid.com>

	Merge trunk version 186243 into gupc branch.
	Incorporates a powerpc 'ffi' fix.

2012-03-16  Gary Funck  <gary@intrepid.com>

	Merge trunk version 185454 into gupc branch.
	Incorporates a libgcc fix for builds on Darwin.

2012-03-13  Gary Funck  <gary@intrepid.com>

	Merge trunk version 185278 into gupc branch.
	* c-decl.c (c_build_pointer_type): For UPC pointer-to-shared types
	call build_pointer_type() to apply UPC-specific qualifiers.
	* top-level/configure.ac: factor the checking for posix hostst
	out of the libgomp section so that it can also be used by libgupc.
	* top-level/configure: Re-generate.
	* DEV-PHASE: bump to 4.8.0-1.

2012-03-12  Gary Funck  <gary@intrepid.com>

	* config/rs6000/rs6000.c (rs6000_function_value): Do not over-ride
	the mode for a pointer for upc-pointer-to-shared types.

2012-03-04  Gary Funck  <gary@intrepid.com>

	Merge trunk version 184900 into gupc branch.

2012-02-17  Gary Funck  <gary@intrepid.com>

	Released GUPC 4.7.0.2 based on version 183992.
	* DEV-PHASE: Bump release identifier to 4.7.0-3.
	* DATESTAMP: Bump date stamp.

2012-02-07  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/gupc.texi: Added entries for the directory.

2012-02-05  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/gupc-manpage.html: Removed. HTML files are generated from
	the texi source.

2012-02-05  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/gupc.texi (-x upc): Add files ending with '.c' to the list
	of files compiled as UPC source.

2012-02-04  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/Make-lang.in (doc/gupc.info): Fix dependencies for creating
	gupc.info document.

2012-02-03  Nenad Vukicevic  <nenad@intrepid.com>

	* c-family/c-pragma.c (disable_pupc_mode): New. Disable profiling
	code generation (same as #pragma pupc off).
	(set_pupc_mode): New. Set/restore profiling mode.
	* c-family/c-upc.h (disable_pupc_mode): New. Prototype.
	(set_pupc_mode): New. Prototype.
	* upc/upc-act.c (upc_write_init_func): Disable emitting of the
	profiling code for shared variables initialization routines.

2012-02-03  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/upc-act.c (upc_write_init_func): Disable emitting of the
	profiling code for shared variables initialization routines.

2012-02-03  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/Make-lang.in: Various changes related to generating
	man/info pages from texi source file.
	(upc.install-common): Fix for removing the upc command
	link before install if suffix was applied.
	(upc.install-pdf): New.
	(upc.install-html): Change for the build from texi.
	(upc.install-man): Change for the build from texi.
	* upc/gupc.1: Removed.
	* upc/gupc.texi: New. Created texi source for man/info
	generation.

2012-01-31  Gary Funck  <gary@intrepid.com>

	Merge trunk version 183751 into gupc branch.
	Incorporates fix for bootstrap failure on openSUSE 12.1.

2012-01-30  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/Make-lang.in: Fix the --program-suffix configuration
	option.  GUPC executables are now installed with appropriate
	suffixes and GUPC driver execs 'gcc' with the right suffix.

2012-01-24  Nenad Vukicevic  <nenad@intrepid.com>

	* upc/upc-lang.c (upc_init_options): Disable section anchors
	for UPC language.

2012-01-14  Gary Funck  <gary@intrepid.com>

	* upc/upc-genericize.c (upc_genericize_expr): Update input_location
	while traversing the program tree.
	(upc_expand_get, upc_expand_put): For profiling,  derive
	the source code location from the incoming 'loc' parameter.

2012-01-10  Gary Funck  <gary@intrepid.com>

	Merge trunk version 183072 into gupc branch.
	Incorporates libcpp __BASE_FILE__ fix.

2012-01-09  Gary Funck  <gary@intrepid.com>

	* upc/upc-genericize.c (upc_expand_put): Use is_gimple_reg instead of
	is_gimple_non_addressable.

2012-01-06  Nenad Vukicevic <nenad@intrepid.com>

	* upc/Make-lang.in: Add appropriate linker flags when linking
	gupc drivers.

2012-01-06  Nenad Vukicevic <nenad@intrepid.com>

	* upc/Make-lang.in: Add appropriate linker flags when linking
	gupc drivers.

2011-12-31  Gary Funck  <gary@intrepid.com>

	Fix gupc driver to avoid segfault when processing
	invalid use of a switch that expects an argument.
	* upc/gupc.c (get_libgupc_path): Add check for non-NULL value of
	libgupc_archive before attempting to access libgupc_archive[0].
	(main): Do not issue error if lib_dir is NULL.  Instead, only
	process lib_dir if it is non-NULL.

2011-12-31  Gary Funck  <gary@intrepid.com>

	Improve -fupc-debug support.
	* upc/upc-genericize.c (upc_expand_get, upc_expand_put,
	upc_genericize_sync_stmt): Add check for flag_upc_debug.
	* upc/upc-pts-struct.c (upc_pts_struct_build_cvt): Ditto.
	* upc/upc-pts-packed.c (upc_pts_packed_build_cvt): Ditto.
	* upc/upc-act.c (upc_cpp_builtins): Disable inlining of the
	runtime if flag_upc_debug is asserted.

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

2011-11-22  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_cpp_builtins): Generate new pre-defined
	macro: __GUPC__.

2011-11-18  Gary Funck  <gary@intrepid.com>

	Merge trunk version 181552 into gupc branch.
	Incorporates libgcc/libunwind fix for IA64.

2011-11-19  Gary Funck  <gary@intrepid.com>

	* config/rs6000/rs6000.c (rs6000_output_function_epilogue):
	Add check for UPC when defining the language type value
	in a traceback entry.

2011-11-19  Gary Funck  <gary@intrepid.com>

	* upc/config-lang.in: Remove checks for supported targets.
	This is now done at a higher level.

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

2011-10-26  Gary Funck  <gary@intrepid.com>

	Rename "GCC/UPC" to "GNU UPC", "UPC" to "GUPC", and
	"libupc" to "libgupc".
	* upc/gupc.c: Rename from upc-cmd.c and adjust for rename
	of libupc to libgupc.
	* upc/upc-lang.c (LANG_HOOKS_NAME): Change "GCC UPC" to "GNU UPC".
	* upc/config-lang.in: Adjust for rename of libupc to libgupc.
	* upc/gupc.1: Rename from upc.1.  Change "GCC UPC" references
	to "GNU UPC".  Adjust for rename of libupc to libgupc.
	Improve formatting.
	* upc/gupc-manpage.html: Rename from upc-manpage.html.  Re-generate.
	* upc/Make-lang.in: Change "upc" to "gupc".  Change "xupc" to "xgupc".
	Adjust for rename of libupc to libgupc.  Install target symlink from
	"upc" to "gupc".  Install target/version-specific hard links to "gupc".

2011-10-20  Gary Funck  <gary@intrepid.com>

	Merge trunk version 180276 into gupc branch.
	Incorporates fix for PR bootstrap/50709.

2011-10-20  Gary Funck  <gary@intrepid.com>

	Merge trunk version 180246 into gupc branch.

2011-10-19  Gary Funck  <gary@intrepid.com>

	Merge trunk version 180233 into gupc branch.
	Incorporates fix for PR debug/49310 (var tracking).

2011-10-17  Gary Funck  <gary@intrepid.com>

	Fix a regression caused by the previous commit.
	* upc/upc-genericize.c (upc_genericize_walk):
	Renamed from: upc_genericize_stmt.
	(upc_shared_addr): for COMPONENT_REF and INDIRECT_REF
	re-walk the tree after simplification, by calling
	upc_genericize_walk().
	(upc_genericize_array_ref): expand the newly constructed
	indirect reference by calling upc_genericize_indirect_ref().

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

2011-10-10  Gary Funck  <gary@intrepid.com>

	* upc/upc-genericize.c (upc_simplify_shared_ref): When simplifying
	the base address always convert to (shared [] char *).
	This ensures that &a[i].field1 ends up with the required
	zero block size, for example.

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
	* c-family/c-common.h (upc_cpp_builtins,
	  upc_write_global_declarations): Remove extern definitions.
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
	  URL.  Update "man" page to reflect debugging switches.
	  Also, some general clean up.  Change the bug reporting
	  URL to point to gccupc.org.

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

2011-09-22  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_get_block_factor): test TYPE_HAS_BLOCK_FACTOR()
	before calling TYPE_BLOCK_FACTOR().  This fits better with
	recent encoding of block factor values with a hash table.

2011-09-15  Nenad Vukicevic <nenad@intrepid.com>

	Add to FLAGS the flags needed to disable inlining of
	UPC run-time access routines.
	* testsuite/lib/target-supports.exp
	(add_options_for_upc_library_calls): New.

2011-09-15  Gary Funck  <gary@intrepid.com>

	Fix ICE involving shared bit field accesses.
	* tree.c (build3_stat): Propagate TEEE_SHARED()
	TREE_STRICT() and TREE_RELAXED() flags, if applicable.

2011-09-15  Gary Funck  <gary@intrepid.com>

	Fix ICE involving shared bit field accesses.
	* upc/upc-genericize.c (upc_simplify_shared_ref): Improve
	"shared bit fields not yet implemented" error message
	by adding file/line number location.

2011-09-13  Gary Funck  <gary@intrepid.com>

	Merge trunk version 178795 into gupc branch.
	Incorporates fix to PR bootstrap/50010 for x86-32.

2011-09-13  Gary Funck  <gary@intrepid.com>

	* upc/upc-tree.def (UPC_FORALL_STMT): Fix spelling error in comments.
	* upc/upc-lang.c (upc_init_options): Ditto.
	* upc/upc-genericize.c (lookup_unshared_var, upc_shared_addr_rep,
	upc_genericize_expr, upc_genericize_compound_expr): Ditto.
	* upc/upc-act.c (upc_parse_init, upc_build_pointer_type,
	upc_block_factor_insert, upc_pts_is_valid_p): Ditto.

2011-09-08  Gary Funck  <gary@intrepid.com>

	Merge trunk version 178557 into gupc branch.

2011-09-07  Gary Funck  <gary@intrepid.com>

	Ensure that UPC pointer-to-shared type alignment is
	propagated to the final type.  Revert to long-standing
	alignment policy: twice the size of a "C" pointer.
	* tree.c (build_pointer_type): Propagate the alignment
	of the UPC pointer-to-shared representation type
	into the newly built pointer type.

2011-09-07  Gary Funck  <gary@intrepid.com>

	Ensure that UPC pointer-to-shared type alignment is
	propagated to the final type.  Revert to long-standing
	alignment policy: twice the size of a "C" pointer.
	* upc/upc-pts-struct.c (upc_pts_struct_init_type): Ensure that
	shared pointers have twice the alignment of a pointer.
	* upc/upc-act.c (upc_cpp_builtins): Unconditionally emit
	the definition of __UPC_PTS_ALIGN__.

2011-09-02  Gary Funck  <gary@intrepid.com>

	Align UPC pointers-to-shared, only if the target enforces
	strict alignment.
	* upc/upc-pts-struct.c (upc_pts_struct_init_type): Align a
	UPC pointer-to-shared type, only if the target requires
	strict alignment.
	* upc/upc-act.c (upc_cpp_builtins): Output pre-defined macro,
	__UPC_PTS_ALIGN__, only if the target requires strict alignment.

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

2011-08-29  Gary Funck  <gary@intrepid.com>

	Implement a hash table to record UPC block factors.
	* upc/upc-act.c (upc_block_factor_for_type): New garbage-collected
	hash table.
	(upc_lang_init, upc_finish): Move to end of source file.
	(upc_lang_init): create  upc_block_factor_for_type hash table.
	(upc_block_factor_lookup, upc_block_factor_insert): New.
	(upc_grok_layout_qualifier, upc_lang_layout_decl, upc_pts_int_sum):
	Rename UPC_TYPE_HAS_THREADS_FACTOR() to TYPE_HAS_THREADS_FACTOR().
	(upc_grok_layout_qualifier): convert blocking factor to sizetype
	before checking for equality to element type's blocking factor.

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

2011-08-28  Gary Funck  <gary@intrepid.com>

	Re-work the type machinery to fully support and to unify support
	for the UPC layout qualifier (blocking factor).
	* upc/upc-genericize.c (upc_simplify_shared_ref): Call
	c_build_qualified_type_1() instead of upc_set_block_factor()
	to assert a zero blocking factor.
	* upc/upc-pts-struct.c (upc_pts_struct_init_type): Call
	c_build_qualified_type_1() to build predefined shared
	qualified types, instead of build_variant_type_copy().
	* upc/upc-pts-packed.c (upc_pts_packed_init_type): Call
	c_build_qualified_type_1() to build predefined shared
	qualified types, instead of build_variant_type_copy().
	* upc/upc-act.c (upc_set_block_factor): Delete.
	(upc_grok_layout_qualifier): Rename from upc_apply_layout_qualifier().
	Rework logic so that it returns a blocking factor rather than
	a qualified type.  Add 'loc' argument and call error_at().
	Add an ELEM_BLOCK_FACTOR argument and Implement logic that merges
	the block size of the element type into the result type, and checks
	for errors due to an attempt to merge differing blocking factors.

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

2011-08-16  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_decl_init): Do not attempt to expand
	  an error mark node.  Fixes ICE after error diagnostic.
	* upc/upc-cmd.c (main): detect missing option argument for
	  options that require an argument, and print error diagnostic.
	  (get_print_cmd): Re-direct error output to /dev/null to
	  avoid issuing duplicate error messages.

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

2011-08-12  Gary Funck  <gary@intrepid.com>

	Rework/simplify the UPC genericize pass.
	* upc/upc-pts.h (upc_pts_ops_t): Remove 'add_offset' field.
	* upc/upc-act.h: Cosmetic change.
	* upc/config-lang.in (gtypes): Add upc-genericize.[ch] to the list.
	* upc/upc-genericize.c (upc_expand_get):
	  Add WANT_STABLE_VALUE parameter.
	  (upc_build_shared_var_addr, upc_shared_addr_rep): New.
	  (uid_tree_map, unshared_vars, uid_tree_map_hash, uid_tree_map_eq,
	  create_unshared_var, lookup_unshared_var, map_unshared_var,
	  unshared_var_addr, unshared_var_name,
	  upc_free_unshared_var_table): Move from upc-act.c.
	  (upc_expand_get, upc_expand_put): Change their source and/or
	  destination parameters so that they are no longer addresses
	  of the objects, but rather references to the objects themselves.
	  (upc_simplify_shared_ref): Rewrite (simplify/generalize).
	  (upc_shared_addr_rep): New.
	  (upc_shared_addr):  Adjust call to upc_build_shared_var_addr(),
	  and re-factor the logic.
	  (upc_genericize_sync_stmt): Re-format comment.
	  (upc_genericize_shared_var_ref): Adjust call to upc_expand_get().
	  (upc_genericize_addr_expr): Re-factor the logic.
	  (upc_genericize_indirect_ref, upc_genericize_field_ref):
	  Adjust call to upc_expand_get() and re-factor the logic.
	  (upc_genericize_modify_expr): Adjust call to upc_expand_put()
	  and re-factor the logic.
	  (upc_genericize_expr): Improve/fix comments.
	  (upc_genericize_finish, upc_genericize_init): New.
	* upc/upc-pts-struct.c (upc_pts_struct_build_add_offset): Delete.
	  (upc_char_pts_type_node): Create new global type node.
	  (upc_pts_struct_is_null_p): Generalize variable names to
	  reflect the fact that the vaddr field can be either first/last.
	  (upc_pts_struct_build_sum): Make corrections to comments.
	  (upc_pts_struct_build_add_offset): Delete.
	* upc/upc-genericize.h (upc_genericize_finish, upc_genericize_init):
	  New prototypes.
	* upc/upc-pts-packed.c (upc_pts_packed_build_add_offset): Delete.
	  (upc_char_pts_type_node): Create new global type node.
	* upc/Make-lang.in: Add dependencies to gtype-upc.h
	  and gt-upc-upc-genericize.h.
	* upc/upc-act.c: Adjust includes to reflect moving the functions
	  that handle the UPC unshared "shadow variables" into
	  upc-genericize.c.  Improve/fix various comments.
	* upc/upc-act.c (upc_parse_init): Call upc_genericize_init().
	  (upc_build_pointer_type): New.
	  (upc_set_block_factor): Re-purpose.  Move the front-end
	  related error checks into upc_apply_layout_qualifier().
	  (upc_apply_layout_qualifier): New.
	  (upc_write_global_declarations): Call upc_genericize_finish().

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

2011-07-11  Gary Funck  <gary@intrepid.com>

	* upc/upc-genericize.c (upc_expand_put): Fix bug, where strict/relaxed
	qualification was incorrectly derived from the source operand.
	(upc_genericize_fndecl): New.
	(upc_gnericize): Call upc_genericize_fndecl() to avoid calling
	c_genericize() more than once in the event of nested procedures.
	(upc_genericize_real_imag_ref): Rename,
	was: upc_genericize_real_image_ref.
	(upc_expand_put): call internal_error() with meaningful message
	in lieu of abort().
	(upc_genericize_real_imag_ref): call internal_error() with
	meaningful message in lieu of gcc_unreachable().

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

2011-07-06  Gary Funck  <gary@intrepid.com>

	Improve/simplify the logic in upc_genericize.
	* upc/upc-genericize.c (upc_create_tmp_var): Delete the
	  PREFIX argument.  Aways use "UPC" as the prefix.
	  (upc_genericize_compound_expr): Add a WANT_VALUE argument.
	  (upc_genericize_cond_expr): New.
	  (upc_genericize_decl_expr): New.
	  (upc_genericize_stmt): New.
	  (upc_copy_value_to_tmp_var, upc_expand_get): Adjust call
	  to upc_create_tmp_var().
	  (upc_genericize_expr): Change the handling
	  of the want_value flag passed in the DATA argument.
	  Always assert this flag after processing EXPR_P.
	  (upc_genericize_compound_expr): Adjust call
	  to upc_genericize_compound_expr() and call
	  upc_genericize_cond_expr() and upc_genericize_decl_expr().

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
	* upc/upc-genericize.c: Rename upc-gimplify.c to upc-genericize.c.
	* upc/upc-genericize.h: Rename upc-gimplify.h to upc-genericize.h.
	* upc/Make-lang.in: Adjust to refer to upc-genericize.[cho].
	* upc/upc-lnag.c: refer to upc-genericize.h.
	  (LANG_HOOKS_GENERICIZE): define as upc_genericize.
	  (LANG_HOOKS_GIMPLIFY_EXPR): Delete.
	  (LANG_HOOKS_INSTRUMENT_FUNC): Delete.
	* upc/upc-act.c: Minor code format fix.

2011-06-30  Gary Funck  <gary@intrepid.com>

	* c-family/c.opt: Bring options definitions up-to-date
	with respect to changes made in the trunk.

2011-06-29  Gary Funck  <gary@intrepid.com>

	Merge trunk version 175584 into gupc branch.

2011-06-29  Gary Funck  <gary@intrepid.com>

	Merge trunk version 175584 into gupc branch.
	* upc/upc-act.c (upc_handle_option): Change reference to
	have_named_sections so that it refers to the
	targetm_common structure.

2011-06-28  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.h (upc_write_init_func, upc_free_unshared_var_table):
	Remove extern definitions. Now declared as 'static'.
	* upc/upc-gimplify.c, upc/upc-lang.c,
	upc-pts-struct.c, upc-gasp.c, upc-pts-packed.c,
	upc-cmd.c, upc-act.c: Improve/add comments, fix
	typos and spelling errors.

2011-06-13  Gary Funck  <gary@intrepid.com>

	libcpp/
	* include/cpplib.h (enum c_lang):
	Move the entry for CLK_UPC so that it follows CLK_STDC1X.
	This keeps all the "C" variants together.
	* init.c (lang_defaults): Add an entry for UPC.

2011-06-10  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_set_block_factor): Handle the case where
	the UPC blocking factor expression overflowed.

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

2011-06-02  Gary Funck  <gary@intrepid.com>

	Merge trunk version 174558 into gupc branch.
	* upc/upc-lang.c (upc_check_decl_init): Use recently defined
	'append_to_statement_list_force' function to add UPC
	initialization statements to the UPC initialization
	statement list ('upc_init_stmt_list').
	(upc_build_init_func): Use recently defined
	'append_to_statement_list_force' function to add
	statements listed in 'upc_init_stmt_list' onto the
	function body constructed to implement initialization
	of UPC declarations that require active initialization
	at program start up.  The previous methods of manipulating
	statement lists no longer worked, due to changes in the
	the statement list structure.

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

2011-05-23  Gary Funck  <gary@intrepid.com>

	* upc/upc-pts-struct.c: Revert to code that aligned the
	  internal pointer-to-shared representation to
	  twice the alignment of a pointer.  This fixes
	  an ICE that occurred when building the compiler
	  on an IA64 target.

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

2011-05-17  Gary Funck  <gary@intrepid.com>

	* upc/upc-pts.h (upc_pts_build_value, upc_pts_build_add_offset,
	  upc_pts_build_cond_expr, upc_pts_build_constant,
	  upc_pts_build_cvt, upc_pts_build_diff, upc_pts_build_sum,
	  upc_pts_build_threadof, upc_pts_init_type): Remove extern
	  definitions.
	  (upc_pts_ops_t): New. Define handler table that will
	  implement either the 'packed' or the 'struct' representation
	  of a UPC pointer-to-shared value.
	  (upc_pts_packed_ops, upc_pts_struct_ops): New. Define extern that
	  refers to the packed and the struct UPC pointer-to-shared
	  representation implementation.
	  (upc_pts): New. Define handler table that is set up at initialization
	  to refer to the handlers for the UPC pointer-to-shared
	  representation (packed/struct) that has been configured.
	  Add conditional compilation test for HAVE_UPC_PTS_PACKED_REP
	  and configure the UPC pointer-to-shared definitions that
	  are specific to the configured UPC pointer-to-shared
	  representation.
	* upc/upc-act.h (upc_pts_is_valid_p): Rename extern definition from
	  is_valid_pts_p to upc_pts_is_valid_p.
	* lang-specs.h: Reformat the UPC compilation specs.
	  to improve readability.
	* upc/upc-rts-names.h: New. Define the names of UPC runtime
	  library functions that implement UPC language statement
	  semantics.  These definitions were moved from config/upc-conf.h
	* upc/upc-gimplify.c: Add include of "upc-rts-names.h".
	  Refer to newly defined pointer manipulation routines
	  by indirecting through the newly defined "pts" handler table.
	  Re-format, re-indent.
	* upc/upc-lang.c: Add include of "upc-pts.h".  Fix a comment.
	* upc/config-lang.in: Remove logic that inserted "config/upc-conf.h"
	  into the target include and file path.
	* upc/upc-pts-struct.c: Add include of "upc-rts-names.h".
	  (upc_pts_struct_ops): Define the pointer manipulation
	  handler table that implements operations on UPC
	  pointers-to-shared, represented as a struct.
	  Re-format and re-indent.
	* upc/upc-gasp.c: Add include of "upc-rts-names.h".
	  Re-format and re-indent.
	* upc/upc-pts-packed.c: Add include of "upc-rts-names.h".
	  (upc_pts_packed_ops): Define the pointer manipulation
	  handler table that implements operations on UPC
	  pointers-to-shared, represented as a packed integer.
	  Re-format and re-indent.
	* upc/Make-lang.in: Compile both "upc-pts-packed.c" and
	  "upc-pts-struct.c".  One/other will be selected at
	  compilation time to implement operations on UPC
	  on the UPC pointer-to-shared that has been configured.
	  Add dependencies on "upc-rts-names.h".
	* upc/upc-cmd.c: Re-format and re-indent.
	* upc/upc-act.c: Add include of "upc-rts-names.h".
	  (upc_pts): New. Add definition of the handler
	  table that implements representation specific
	  operations on trees that refer to UPC pointer-to-shared
	  objects and types.  Re-format and re-indent.

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
	* c-family/c-common.h (upc_cpp_builtins,
	  upc_write_global_declarations): Remove extern definitions.
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

2011-05-06  Gary Funck  <gary@intrepid.com>

	Eliminate compilation warnings, by fixing
	#include's and updating function prototypes.
	* upc/upc-gimplify.c: Include bitmap.h.
	  (upc_gimplify_real_image_ref): Mark various parameters
	  as unused.  This is a stub procedure for now.
	* upc/upc-lang.c: Include c-family/c-upc.h.
	  upc-gasp.c: Ditto.
	* upc/upc-cmd.c (no_start_files): Delete unused variable.
	  The -nostartfiles switch is now handled by the linker spec.
	* ChangeLog: Spell check.

2011-05-06  Gary Funck  <gary@intrepid.com>

	Upgrade c-family source files to conform with modularity
	improvements.  Mainly, remove #include of c-tree.h in files
	under c-family, and define a new UPC-specific #include file,
	c-upc.h, and use it.
	* upc/upc-act.h (upc_write_global_declarations,
	  upc_check_decl, upc_build_sync_stmt, upc_affinity_test,
	  upc_num_threads, upc_diagnose_deprecated_stmt):
	  Move external definitions to c-family/c-upc.h
	  (upc_blocksizeof_expr, upc_blocksizeof_type,
	  upc_elemsizeof_expr, upc_elemsizeof_type,
	  upc_localsizeof_expr, upc_localsizeof_type):
	  Remove external definitions; these functions
	  were moved from upc-act.c to c-parser.c.
	* upc/upc-gimplify.c: Add #include of c-family/c-upc.h.
	  upc-pts-packed.c: Ditto.
	  upc-pts-struct.c: Ditto.
	* upc/config-lang.in: Update gtfiles to refer to c-upc.h
	  and other files.
	* upc/upc-act.c: Add #include of c-family/c-upc.h.
	  (upc_blocksize, upc_elemsizeof, upc_localsizeof):
	  Make external so that it can be called from c-parser.c.
	  (upc_blocksizeof_expr, upc_blocksizeof_type,
	  upc_elemsizeof_expr, upc_elemsizeof_type,
	  upc_localsizeof_expr, upc_localsizeof_type):
	  Move from upc/upc-act.c to c-parser.c.
	  (upc_set_block_factor): Fix typo in error message.
	  (upc_shared_type_p, upc_pts_cvt_op_p): Delete.
	  Move to tree.h and define as a macro.
	  (upc_get_unshared_type): Delete. renamed to
	  build_upc_unshared_type and moved to tree.c.
	* upc/upc-gimplify.c (upc_gimplify_lval, upc_gimplify_expr):
	  refer to renamed build_upc_unshared_type function.
	  upc-act.c (create_unshared_var): Ditto.

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

2011-05-03  Gary Funck  <gary@intrepid.com>

	* upc/upc-lang.c: Remove extraneous FIXME/TODO comments.
	* upc/upc-pts-packed.c (upc_pts_build_cvt): Ditto.
	* upc/upc-act.c (upc_set_block_factor): Ditto.

2011-04-29  Gary Funck  <gary@intrepid.com>

	* c-decl.c (finish_decl): Improve error diagnostics.
	  (grokdeclarator): Ditto.
	* c-typeck.c (build_c_cast): Improve error diagnostics.
	  (convert_for_assignment): Ditto.
	  (build_binary_op): Ditto.
	* c-parser.c (c_parser_upc_forall_statement):
	  Improve error diagnostics.
	* convert.c (convert_to_integer):  Improve error diagnostics.

2011-04-29  Gary Funck  <gary@intrepid.com>

	* upc/upc-gimplify.c (upc_expsnd_get): Improve error diagnostics.
	  (upc_expand_put): Ditto.
	  (upc_shared_addr): Ditto.
	  (upc_gimplify_sync_stmt): Ditto.
	  (upc_gimplify_field_ref): Ditto.
	* upc/upc-pts-struct.c (upc_pts_build_diff): Improve error diagnostics.
	  (upc_pts_build_cvt): Ditto.
	* upc/upc-act.c (upc_handle_option): Improve error diagnostics.
	  (upc_lang_init): Ditto.
	  (upc_sizeof_type_check): Ditto.
	  (upc_set_block_factor): Ditto.
	  (upc_decl_init): Ditto.
	  (upc_affinity_test): Ditto.
	  (upc_num_threads): Ditto.
	  (upc_diagnose_deprecated_stmt): Ditto.
	  (upc_build_shared_var_addr): Ditto.
	  (upc_pts_int_sum): Ditto.
	  (upc_pts_diff): Ditto.

2011-04-28  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (create_unshared_var): call upc_get_unshared_type()
	  instead of using TYPE_MAIN_VARIANT() to create an equivalent type
	  that is not a UPC shared type.  This is sometimes necessary
	  when the given shared type is derived from a typedef.

2011-04-24  Gary Funck  <gary@intrepid.com>

	* c-parser.c (c_parser_upc_sync_statement): Fix ICE that
	  occurred if there is an error in the barrier id
	  expression.  Map error_mark_node into NULL.

2011-04-24  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_handle_option): Fix incorrect error
	  diagnostic messages when both -fupc-debug and
	  -fupc-inline-lib are asserted.

2011-04-24  Gary Funck  <gary@intrepid.com>

	* upc/upc-cmd.c (GCC_WORD_SWITCH_TAKES_ARG): Add "--param"
	  to the list of switches that accept arguments.
	  (main): Check for switches that have a following
	  argument inside the loop that copies arguments and
	  adds '-x upc' or '-x none' as necessary.
	  (main): Misc. clean ups and simplifications.

2011-04-22  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172873 into gupc branch.

2011-04-22  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172873 into gupc branch.
	* upc/upc-act.c (upc_build_init_func): assert DECL_PRESERVE_P()
	  on init_func() to prevent it from being removed from
	  the call graph.

2011-04-19  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172359 into gupc branch.

2011-04-19  Gary Funck  <gary@intrepid.com>

	Merge trunk version 172359 into gupc branch.
	* upc/upc-lang.c (upc_init_ts): New.
	  (LANG_HOOKS_INIT_TS): use upc_init_ts.

2011-04-19  Gary Funck  <gary@intrepid.com>

	Eliminate warnings when compiling upc-cmd.c.
	* upc/upc-cmd.c (file_exists): Remove.
	  (arg_copy): Remove const qualifier.

2011-04-14  Gary Funck  <gary@intrepid.com>

	* c-family/c.opt: Add UPC as a valid language for various
	  switches where it is valid for "C", that were not updated
	  in previous merges with trunk.

2011-04-13  Gary Funck  <gary@intrepid.com>

	* c-family/c.opt (fupc-pre-include): New option definition.
	* gcc.c (upc_options): Do not add "-include gcc-upc.h" if
	  -fno-upc-pre-include is asserted.

2011-04-13  Gary Funck  <gary@intrepid.com>

	* upc/upc-cmd.c (GCC_WORD_SWITCH_TAKES_ARG): Add "dumpbase"
	  to the list, and alphabetize.

2011-04-13  Gary Funck  <gary@intrepid.com>

	* upc/upc-gimplify.c (upc_gimplify_real_image_ref): New.
	  Currently, a not-yet-implemented stub.
	  (upc_gimplify_lval): call upc_gimplify_real_image_ref to
	  rewrite UPC shared REALPART_EXPR and IMAGPART_EXPR lvalues.
	  This will avoid an ICE when compiling regular "C"
	  code that refers to those operators.

2011-04-13  Gary Funck  <gary@intrepid.com>

	* upc/upc-cmd.c: Do not add "-isystem <libupc_path>" if
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

2011-03-22  Gary Funck  <gary@intrepid.com>

	* upc/upc-cmd.c: Move linker switches into libupc/libupc.spec.
	  (UPC_LINKER_SCRIPT, LIBNUMA, LIBUPC, LIBUPC_PT) Remove.
	  (find_ld_script) Remove.

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

2011-03-20  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c: Call error() directly with a format specifier,
	  rather than using sprintf() to format the message.
	  This should make it easier to internationalize UPC's error messages.

2011-03-20  Gary Funck  <gary@intrepid.com>

	Move UPC start files, end files, and linker specs.
	into libupc.  This reduces the impact on common GCC
	configuration files, and ensures that these UPC-specific
	components are only built when the UPC language dialect is selected.
	* upc/config-lang.in: Remove setting of 'upc_extra_parts', which
	  was used to specify which upc-crtbegin and upc-crtend files
	  need to built.  Remove extraneous (commented out)
	  settings of 'cfiles'.  The listed configuration files
	  have been reverted to trunk, and weren't referenced
	  via cfiles in any event.
	* upc/upc-cmd.c: Remove test for HAVE_UPC_LINK_SCRIPT.
	  Simply test for the presence of the UPC link script
	  in the current directory or the libupc directory.
	  Add -B<path-to-libupc> to the switches passed to 'gcc';
	  this is needed in order to find the upc-crtbegin
	  and upc-crtend object files now built in libupc.
	  Remove test for and inclusion of UPC_LINKER_SWITCHES.
	  if extra switches are needed for a particular target
	  (like SGI/Irix), they will be defined by the custom
	  linker spec. built in libupc.  Remove test for
	  HAVE_UPC_NUMA_SUPPORT; if '-lnuma' is needed, it
	  will be added to the custom linker specs. built
	  in libupc.
	* upc/upc-crtstuff.c: Move to libupc.

2011-02-23  Gary Funck  <gary@intrepid.com>

	* c-decl.c (undeclared_variable): fix typo. Inadvertently
	removed negation on following 'if'.

2011-02-22  Gary Funck  <gary@intrepid.com>

	* c-decl.c (undeclared_variable): call upc_diagnose_deprecated_stmt
	to check for the usage of certain deprecated UPC keywords.
	* c-family/stub-upc.c (upc_diagnose_deprecated_stmt): New.
	* c-tree.h (undeclared_variable): Define prototype.

2011-02-22  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c (upc_diagnose_deprecated_stmt): New.
	  upc-act.h (upc_diagnose_deprecated_stmt): Define.
	  Check usage of deprecated keywords and issue
	  error message.

2011-02-12  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: (build_c_cast, convert_for_assignment)
	  Diagnose an attempt to convert from an integer to
	  a pointer-to-shared as an error.  Also, fix various
	  error messages so that they use the preferred term
	  pointer-to-shared instead of "shared pointer".

2011-02-08  Gary Funck  <gary@intrepid.com>

	* upc/upc-pts-struct.c: Fix bug: (pts + int) fails when int is negative
	  for struct-pts representation
	  Analysis indicated that for something like (+ ptr -80) this was
	  being represented as (+ ptr (- 80)) and further, when the
	  calculations were propagated into the individual operations on the
	  components of the 'struct' pointer, it would end up with something
	  like (+ ptr.vaddr (- 80)), and the type of (- 80) would end up as
	  "long unsigned int" because of the addition to the pointer.  This
	  caused the calculations involving the signed 'int' operand to be
	  performed incorrectly.  This fix insures that the 'int' operand is
	  signed.

2011-02-08  Gary Funck  <gary@intrepid.com>

	* upc/config-lang.in, upc/lang-specs.h, upc/Makefile.in,
	  upc/Make-lang.in, upc/upc.1, upc/upc-act.c, upc/upc-act.h,
	  upc/upc-cmd.c, upc/upc-crtstuff.c, upc/upc-gasp.c,
	  upc/upc-gasp.h, upc/upc-gimplify.c, upc/upc-gimplify.h,
	  upc/upc-lang.c, upc/upc-pts.h, upc/upc-pts-packed.c,
	  upc/upc-pts-struct.c, upc/upc-tree.def, upc/upc-tree.h:
	Update copyright notices.

2011-02-07  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: (convert_for_assignment)
	  Fix typo. in error message.

2011-01-23  Gary Funck  <gary@intrepid.com>

	* c-typeck.c: (c_build_qualified_type)
	  derive UPC block size by calling upc_get_block_size(),
	  to ensure that the element type of an array of an array
	  is derived correctly.

2011-01-09  Gary Funck  <gary@intrepid.com>

	Fix behavior of upc_localsizeof() when called in a dynamic
	threads compilation environment.  This bug, and the expected
	behavior is documented in this bug report:
	https://upc-bugs.lbl.gov/bugzilla/show_bug.cgi?id=2960.
	* upc/_act.c (upc_localsizeof): Re-implement.

2010-12-29  Gary Funck  <gary@intrepid.com>

	Merge trunk version 168314 into gupc branch.

2010-12-15  Gary Funck  <gary@intrepid.com>

	Merge trunk version 167307 into gupc branch.

2010-12-15  Gary Funck  <gary@intrepid.com>

	Merge trunk version 167307 into gupc branch.
	* upc/_act.h: (upc_handle_option) add location and struct
	cl_option_handlers * parameters.
	(upc_finish_file) Remove.
	(upc_write_global_declarations) New, replaces upc_finish_file.
	* lang-spec.h: Disable multi-file compilation for .upc files.
	This fixes an issue where file-scoped static variables were
	diagnosed as multiply-defined.  Both "C" and "ObjC" also disable
	multi-file compilation.
	* upc/upc-gimplify.c: (upc_expand_put) Check for INDIRECT_REF_P()
	explicitly when deciding whether a UPC shared object is addressable.
	is_gimple_addressable() used to do this, but now checks for MEM_REF
	which does not apply to UPC shared objects.
	(upc_gimplify_lval, upc_gimplify_expr) Delete references to
	ALIGN_INDIRECT_REF and MISALIGNED_INDIRECT_REF.
	These are no longer defined.
	(upc_genericize) Add call to bitmap_obstack_initialize()
	and bitmap_obstack_release() around call to gimplify_function_tree().
	* upc/upc-lang.c: Add #include of "opts.h" and "options.h".
	(flag_upc_debug, flag_upc_inline_lib, flag_upc_instrument,
	flag_upc_instrument_functions) Remove.  Use definitions
	generated by the options file.
	(upc_init_options) Use cl_decoded_option struct.
	(LANG_HOOKS_WRITE_GLOBALS) Define as upc_write_global_declarations.
	(finish_file) Delete.
	(upc_init_options) Call control_warning_option() to specify
	-Werror=pointer-arith as the default.  Remove call to
	enable_warning_as_error().
	* upc/upc-pts-struct.c: (upc_pts_init_type) Move test that
	UPC_PTS_THREAD_SIZE is a multiple of a byte into an "if" statement
	rather than an #ifdef; this macro now depends upon a
	target size macro which must be evaluated at runtime.
	* upc/Make-lang.in: (cc1-dummy) Remove make target.
	(cc1upc-checksum.c) Generate directly from object files.
	* upc/upc-cmd.c: (SWITCH_TAKES_ARG, WORD_SWITCH_TAKES_ARG) Delete
	references to these deprecated macro definitions.
	(GCC_WORD_SWITCH_TAKES_ARG, GCC_WORD_SWITCH_TAKES_ARG) New.
	(all_exec_args) Delete variable.
	(exec_args) Re-define as (const char *).
	(exec_arg_list) New.  Make copy of exec_args to pass to 'exec'.
	* upc/upc-act.c: (upc_handle_option) Update argument list to accept
	(cl_option_handlers *) argument.
	(upc_finish_file) Rename to upc_write_global_declarations.
	(upc_write_global_declarations) New.

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

2010-10-17  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c: Issue a compilation error on attempts to perform
	  arithmetic on generic pointer-to-shared values.
	  (The rest of this update is a small code clean up.)

2010-10-17  Gary Funck  <gary@intrepid.com>

	* upc/upc-lang.c: For UPC only, issue errors for arithmetic and related
	  operations on void types.
	  This update simulates the effect of -Werror=pointer-arith, thus
	  causing arithmetic on void types, sizeof(void) and so on to be
	  considered compilation errors.  This meets the expectations of some
	  harness tests and the RTED/CTED test suites.  GCC is more
	  permissive, but since this is for UPC only, we can fairly safely
	  define this new policy.

2010-10-16  Gary Funck  <gary@intrepid.com>

	* Makefile.in: Implement fixes for SGI/IRIX/MIPS port.
	  The gcc/Makefile.in rules for install-plugin had to be re-written to
	  break up a long list of header files that exceeded the command line
	  limitation imposed by Irix.
	  Access functions for TFmode types had to be implemented.
	  Apparently, this is the mode used for the SGI/MIPS port to represent
	  "long float".

2010-10-16  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c: Fix bug: Static initialization of shared
	  arrays is unsupported -- issue an error message.
	  Currently, static initialization of a shared array is not
	  implemented correctly.
	  We do not plan to fix this for a while, therefore the compiler will
	  issue an error message indicating that this is an unsupported
	  operation.

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

2010-10-13  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c: Fix bug: negative layout qualifiers not diagnosed
	  as an error.
	  The CTED_UPC test c_A_1_3_b.upc, specified a negative blocksize, but
	  it was not diagnosed as an error.  With this fix, negative block
	  sizes will generate a translation error.

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

2010-10-10  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c: Fix bug: ICE: Attempt to take the difference of
	  shared and non-shared pointers
	  The compiler detected the error, and then tried to return
	  error_mark_node.  This apparently is not acceptable, as there is an
	  explicit assertion check to prevent this from happening in
	  build_binary_op.  Return the more user-friendly size_one_node
	  instead.

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

2010-10-09  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c: Fix bug: segfault on incomplete array definition.
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
	  See also: gcc/c-common.c gcc/upc/upc-act.c

2010-10-01  Gary Funck  <gary@intrepid.com>

	* configure, configure.ac: Update manual page, and bug reporting
	  URL.
	  Update "man" page to reflect debugging switches.  Also, some general
	  clean up.  Change the bug reporting URL to point to gccupc.org.

2010-10-01  Gary Funck  <gary@intrepid.com>

	* upc/upc-manpage.html, upc.1: Update manual page, and bug reporting
	  URL.
	  Update "man" page to reflect debugging switches.  Also, some general
	  clean up.  Change the bug reporting URL to point to gccupc.org.
	  See also: gcc/configure gcc/configure.ac gcc/upc/upc-manpage.html
	  gcc/upc/upc.1

2010-10-01  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c: Do not zap all qualifier bits when constructing a
	  non-shared result node.
	  This fix was suggested by Paul H. in the following Berkeley Bugzilla
	  report: https://upc-bugs.lbl.gov/bugzilla/show_bug.cgi?id=2061 The
	  use of "!" rather than "~" zapped all the type qualifier bits rather
	  than just those that are related to the "shared" qualifier.  This
	  fix clears only the relevant bits.

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

2010-09-26  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c: Fix bug: upc_forall with empty clauses
	  mis-diagnosed as syntax error.
	  A failure was exhibited by the Berkeley test case,
	  bug873a.upc, which has the code:
	       double d;
	       upc_forall (;;;d) {...} The compiler did not properly handle
	  the empty "condition" clause, and did not recover well when it was
	  determined that the use of a double value, "d" above, was neither a
	  pointer-to-shared nor an integer expression.  The update implements a
	  fix for both issues.
	  See also: gcc/c-parser.c gcc/upc/upc-act.c

2010-09-25  Gary Funck  <gary@intrepid.com>

	* upc/upc-pts-struct.c: Fix an ICE on 32-bit/struct target: failed
	  gimple check when calculating affinity for upc_forall.
	  Intrepid test, test10.upc, failed to compile due a mis-match between
	  the COMPONENT_REF node and the internal 'thread' field.  Changed the
	  code to make the types agree, and added a conversion to sizetype if
	  necessary.

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

2010-09-23  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c: Fix bug: ICE: '[*]' layout factor applied to array
	  with static threads and size not a multiple of threads.
	  Add an extra check for an attempt to apply a '[*]' layout qualifier
	  to a shared array that does not specify a size that is a multiple of
	  THREADS, when compiled in a static THREADS compilation environment.

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

2010-09-22  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c, upc/upc-act.h: Fix bug: Nested upc_forall() semantics
	  are not implemented
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

2010-09-18  Gary Funck  <gary@intrepid.com>

	* upc/upc-act.c, upc/upc-pts-packed.c, upc/upc-pts-struct.c:
	  Implement -fupc-debug switch.

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

2010-07-03  Gary Funck  <gary@intrepid.com>

	Merge trunk version 161517 into gupc branch.
	* upc/config-lang.in: Update references to files
	  moved into c-family/.
	* upc/Make-lang.in: Likewise.
	* upc/upc-act.c: Likewise.
	* upc/upc-gasp.c: Likewise.
	* upc/upc-gimplify.c: Likewise.
	* upc/upc-lang.c: Likewise.
	* upc/upc-pts-packed.c: Likewise.
	* upc/upc-pts-struct.c: Likewise.
	* upc/upc-act.c: (upc_handle_option) add parameters to
	  effect pass through call to c_common_handle_option.
	* upc/upc-act.h: (upc_handle_option) Likewise.
	* upc/upc-act.c: (map_unshared_var) used typed ggc allocation.
	* upc/upc-act.c: (upc_build_init_func) add call to
	  to mark_decl_referenced(), to ensure that UPC shared
	  variable initializer function is not removed from
	  the call tree graph.

2010-03-01 Gary Funck  <gary@intrepid.com>

	Create gupc branch from trunk version 157149.
