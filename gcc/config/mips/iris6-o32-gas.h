/* Definitions of target machine for GNU compiler, for MIPS running IRIX 6
   (O32 ABI) using the GNU assembler.  */

/* Enforce use of O32 linker, irrespective of SGI_ABI environment variable
   and machine type (e.g., R8000 systems default to -64).  Copied from
   iris5gas.h, only adding -32.  The default options -call_shared
   -no_unresolved are only passed if not invoked with -r.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{static: -non_shared} \
%{!static: \
  %{!shared:%{!non_shared:%{!call_shared:%{!r: -call_shared -no_unresolved}}}}} \
%{rpath} -init __do_global_ctors -fini __do_global_dtors \
%{shared:-hidden_symbol __do_global_ctors,__do_global_ctors_1,__do_global_dtors} \
-_SYSTYPE_SVR4 \
-32"
