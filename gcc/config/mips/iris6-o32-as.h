/* Definitions of target machine for GNU compiler, for MIPS running IRIX 6
   (O32 ABI) using the SGI assembler.  */

/* Enforce use of O32 linker, irrespective of SGI_ABI environment variable
   and machine type (e.g., R8000 systems default to -64).  Copied from
   iris5.h, only adding -32.  The default options -call_shared -no_unresolved
   are only passed if not invoked with -r.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{static: -non_shared} \
%{!static: \
  %{!shared:%{!non_shared:%{!call_shared:%{!r: -call_shared -no_unresolved}}}}} \
%{rpath} \
-_SYSTYPE_SVR4 \
-32"
