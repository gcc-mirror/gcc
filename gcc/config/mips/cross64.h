/* Configuration for an Irix 5 host and Irix 6 target using SGI's cross64
   package.  */

#define STANDARD_INCLUDE_DIR "/usr/cross64/usr/include"
#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/cross64/usr/bin/"
#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/cross64/usr/lib/lib64/"

/* Must add TOOLROOT to the environment, or else the assembler will not
   work.  */
#define INIT_ENVIRONMENT	\
  "TOOLROOT=/usr/cross64"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{mips1:%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}} \
   %{mips2:%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}} \
   %{!mips1:%{!mips2:%{pg:/usr/cross64/usr/lib64/mips4/gcrt1.o}		    \
   %{!pg:%{p:/usr/cross64/usr/lib64/mips4/mcrt1.o			    \
     /usr/cross64/usr/lib64/mips4/libprof1.a}				    \
   %{!p:/usr/cross64/usr/lib64/mips4/crt1.o}}}}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{mips1:crtn.o%s}%{mips2:crtn.o%s}%{!mips1:%{!mips2:/usr/cross64/usr/lib64/mips4/crtn.o}}"

#undef LINK_SPEC
#define LINK_SPEC "\
-64 -_SYSTYPE_SVR4 %{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{!shared: %{!non_shared: %{!call_shared: -call_shared -no_unresolved}}} \
%{!mips1:%{!mips2:-L/usr/cross64/usr/lib64/mips4 -L/usr/cross64/usr/lib64}}"
