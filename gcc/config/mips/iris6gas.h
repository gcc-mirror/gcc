/* Definitions of target machine for GNU compiler.  IRIX 6 with GNU as.  */

/* Override mips.h version to match DWARF 2 default.  */
#undef MDEBUG_ASM_SPEC
#define MDEBUG_ASM_SPEC "%{gstabs*|gcoff*:-mdebug} \
%{!gstabs*:%{!gcoff*:-no-mdebug}}"

/* Override iris6.h version to always use -init/-fini.

   FIXME: integrate those use separate spec/define for this?  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} %{w} \
%{!shared: %{!non_shared: %{!call_shared:%{!r: -call_shared -no_unresolved}}}} \
%{rpath} -init __do_global_ctors -fini __do_global_dtors \
%{shared:-hidden_symbol __do_global_ctors,__do_global_ctors_1,__do_global_dtors} \
-_SYSTYPE_SVR4 -woff 131 \
%{mabi=32: -32}%{mabi=n32: -n32}%{mabi=64: -64}%{!mabi*: -n32}"

/* Disable SHF_MERGE support.  Even if gas supports it, the IRIX 6 O32 ld
   does not without a special elspec(5) file.

   FIXME: Only do this if not using GNU ld.  */
#if HAVE_GAS_SHF_MERGE
#undef HAVE_GAS_SHF_MERGE
#define HAVE_GAS_SHF_MERGE (mips_abi != ABI_32)
#endif /* HAVE_GAS_SHF_MERGE */

/* There's no need to perform collecting with GNU as.  */
#undef COLLECT_PARSE_FLAG
