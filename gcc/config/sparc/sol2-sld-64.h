/* Definitions of target machine for GNU compiler, for 64-bit SPARC
   running Solaris 2 using the system linker.  */

#include "sparc/sol2.h"

/* At least up through Solaris 2.6,
   the system linker does not work with DWARF or DWARF2,
   since it does not have working support for relocations
   to unaligned data.  */

#define LINKER_DOES_NOT_WORK_WITH_DWARF2

/* A 64 bit v9 compiler with stack-bias */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_V9 + MASK_PTR64 + MASK_64BIT /* + MASK_HARD_QUAD */ + \
   MASK_STACK_BIAS + MASK_APP_REGS + MASK_EPILOGUE + MASK_FPU)

/* ??? bi-architecture support will require changes to the linker
   related specs, among perhaps other things (multilibs).  */
/* #define SPARC_BI_ARCH */

/* The default code model.  */
#undef SPARC_DEFAULT_CMODEL
#define SPARC_DEFAULT_CMODEL CM_MEDANY

#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 128

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9
#undef CPP_CPU_DEFAULT_SPEC
#define CPP_CPU_DEFAULT_SPEC "-D__sparcv9"
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC "-xarch=v9"
#undef CPP_CPU_SPEC
#define CPP_CPU_SPEC "\
%{mcypress:} \
%{msparclite:-D__sparclite__} \
%{mf930:-D__sparclite__} %{mf934:-D__sparclite__} \
%{mv8:-D__sparcv8} \
%{msupersparc:-D__supersparc__ -D__sparcv8} \
%{mcpu=sparclet:-D__sparclet__} %{mcpu=tsc701:-D__sparclet__} \
%{mcpu=sparclite:-D__sparclite__} \
%{mcpu=f930:-D__sparclite__} %{mcpu=f934:-D__sparclite__} \
%{mcpu=v8:-D__sparc_v8__} \
%{mcpu=supersparc:-D__supersparc__ -D__sparcv8} \
%{mcpu=v9:-D__sparcv9} \
%{mcpu=ultrasparc:-D__sparcv9} \
%{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:%(cpp_cpu_default)}}}}}}} \
"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
#undef CPP_CPU_DEFAULT_SPEC
#define CPP_CPU_DEFAULT_SPEC "-D__sparcv9"
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC "-xarch=v9a"
#undef CPP_CPU_SPEC
#define CPP_CPU_SPEC "\
%{mcypress:} \
%{msparclite:-D__sparclite__} \
%{mf930:-D__sparclite__} %{mf934:-D__sparclite__} \
%{mv8:-D__sparcv8} \
%{msupersparc:-D__supersparc__ -D__sparcv8} \
%{mcpu=sparclet:-D__sparclet__} %{mcpu=tsc701:-D__sparclet__} \
%{mcpu=sparclite:-D__sparclite__} \
%{mcpu=f930:-D__sparclite__} %{mcpu=f934:-D__sparclite__} \
%{mcpu=v8:-D__sparc_v8__} \
%{mcpu=supersparc:-D__supersparc__ -D__sparcv8} \
%{mcpu=v9:-D__sparcv9} \
%{mcpu=ultrasparc:-D__sparcv9} \
%{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:%(cpp_cpu_default)}}}}}}} \
"
#endif

#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC "\
%{mcpu=v8plus:-xarch=v8plus} \
%{mcpu=ultrasparc:-xarch=v9a} \
%{!mcpu*:%(asm_cpu_default)} \
"

/*
 * This should be the same as in sol2-sld.h, except with "/sparcv9"
 * appended to the paths and /usr/ccs/lib is no longer necessary
 */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} \
   %{b} %{Wl,*:%*} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy %{!mimpure-text:-z text}} \
   %{symbolic:-Bsymbolic -G -dy -z text} \
   %{mcmodel=medlow:-M /usr/lib/ld/sparcv9/map.below4G} \
   %{G:-G} \
   %{YP,*} \
   %{R*} \
   %{compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ucblib/sparcv9:/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{pg:-Y P,/usr/ucblib/sparcv9:/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{!p:%{!pg:-Y P,/usr/ucblib/sparcv9:/usr/lib/sparcv9}}} \
     -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{pg:-Y P,/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{!p:%{!pg:-Y P,/usr/lib/sparcv9}}}} \
   %{Qy:} %{!Qn:-Qy}"

#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/ccs/lib/sparcv9/"
