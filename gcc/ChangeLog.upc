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
