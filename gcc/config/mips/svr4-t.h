/* Definitions of target machine for GNU compiler.  Tandem S2 w/ NonStop UX. */

/* Use the default value for this.  */
#undef STANDARD_INCLUDE_DIR

#undef MACHINE_TYPE
#define MACHINE_TYPE "TANDEM System V.4 Mips"

/* Use the default values in mips.h.  */
#undef MD_STARTFILE_PREFIX
#undef MD_EXEC_PREFIX
#define MD_STARTFILE_PREFIX "/usr/lib/cmplrs/cc/"
#define MD_EXEC_PREFIX "/usr/lib/cmplrs/cc/"

/* These are the same as the ones in svr4-5.h, except that references to
   /svr4/ have been removed.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt1.o%s}}\
	%{ansi:/usr/lib/values-Xc.o%s} \
                          %{!ansi: \
                           %{traditional:/usr/lib/values-Xt.o%s} \
                           %{!traditional:/usr/lib/values-Xa.o%s}}"

#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{!shared: %{!non_shared: %{!call_shared: -non_shared}}}"
