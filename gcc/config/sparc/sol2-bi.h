/* Definitions of target machine for GNU compiler, for bi-arch SPARC
   running Solaris 2 using the system assembler and linker.  */

/* The default code model.  */
#undef SPARC_DEFAULT_CMODEL
#define SPARC_DEFAULT_CMODEL CM_MEDANY

#define AS_SPARC64_FLAG	"-xarch=v9"

#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC	""
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC	AS_SPARC64_FLAG

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plus"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plusa"
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC AS_SPARC64_FLAG "a"
#endif

#if DEFAULT_ARCH32_P
#define DEF_ARCH32_SPEC(__str) "%{!m64:" __str "}"
#define DEF_ARCH64_SPEC(__str) "%{m64:" __str "}"
#else
#define DEF_ARCH32_SPEC(__str) "%{m32:" __str "}"
#define DEF_ARCH64_SPEC(__str) "%{!m32:" __str "}"
#endif

#undef CPP_CPU_SPEC
#define CPP_CPU_SPEC "\
%{mcypress:} \
%{msparclite|mf930|mf934:-D__sparclite__} \
%{mv8:" DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{msupersparc:-D__supersparc__ " DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{mcpu=sparclet|mcpu=tsc701:-D__sparclet__} \
%{mcpu=sparclite|mcpu-f930|mcpu=f934:-D__sparclite__} \
%{mcpu=v8:" DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{mcpu=supersparc:-D__supersparc__ " DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{mcpu=v9|mcpu=ultrasparc:" DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:%(cpp_cpu_default)}}}}}}} \
"

#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC "\
%{mcpu=ultrasparc:" DEF_ARCH32_SPEC("-xarch=v8plusa") DEF_ARCH64_SPEC(AS_SPARC64_FLAG "a") "} \
%{mcpu=v9:" DEF_ARCH32_SPEC("-xarch=v8plus") DEF_ARCH64_SPEC(AS_SPARC64_FLAG) "} \
%{!mcpu=ultrasparc:%{!mcpu=v9:%{mcpu*:" DEF_ARCH32_SPEC("-xarch=v8") DEF_ARCH64_SPEC(AS_SPARC64_FLAG) "}}} \
%{!mcpu*:%(asm_cpu_default)} \
"

#undef CPP_CPU_DEFAULT_SPEC
#define CPP_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" CPP_CPU64_DEFAULT_SPEC "} \
%{!m64:" CPP_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" CPP_CPU32_DEFAULT_SPEC "} \
%{!m32:" CPP_CPU64_DEFAULT_SPEC "} \
")

#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" ASM_CPU64_DEFAULT_SPEC "} \
%{!m64:" ASM_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" ASM_CPU32_DEFAULT_SPEC "} \
%{!m32:" ASM_CPU64_DEFAULT_SPEC "} \
")

/* wchar_t is called differently in <wchar.h> for 32 and 64-bit
   compilations.  This is called for by SCD 2.4.1, p. 6-83, Figure 6-65
   (32-bit) and p. 6P-10, Figure 6.38 (64-bit).  */

#undef WCHAR_TYPE
#define WCHAR_TYPE (TARGET_ARCH64 ? "int" : "long int")

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Same for wint_t.  See SCD 2.4.1, p. 6-83, Figure 6-66 (32-bit).  There's
   no corresponding 64-bit definition, but this is what Solaris 8
   <iso/wchar_iso.h> uses.  */

#undef WINT_TYPE
#define WINT_TYPE (TARGET_ARCH64 ? "int" : "long int")

#undef WINT_TYPE_SIZE
#define WINT_TYPE_SIZE 32

#undef CPP_ARCH32_SPEC
#define CPP_ARCH32_SPEC "\
-D__GCC_NEW_VARARGS__ -Acpu=sparc -Amachine=sparc"
#undef CPP_ARCH64_SPEC
#define CPP_ARCH64_SPEC "\
-D__arch64__ -Acpu=sparc64 -Amachine=sparcv9 -D__sparcv9"

#undef CPP_ARCH_SPEC
#define CPP_ARCH_SPEC "\
%{m32:%(cpp_arch32)} \
%{m64:%(cpp_arch64)} \
%{!m32:%{!m64:%(cpp_arch_default)}} \
"

#undef ASM_ARCH_SPEC
#define ASM_ARCH_SPEC ""

#undef ASM_ARCH32_SPEC
#define ASM_ARCH32_SPEC ""

#undef ASM_ARCH64_SPEC
#define ASM_ARCH64_SPEC ""

#undef ASM_ARCH_DEFAULT_SPEC
#define ASM_ARCH_DEFAULT_SPEC ""

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "startfile_arch",	 STARTFILE_ARCH_SPEC },		  \
  { "link_arch32",       LINK_ARCH32_SPEC },              \
  { "link_arch64",       LINK_ARCH64_SPEC },              \
  { "link_arch_default", LINK_ARCH_DEFAULT_SPEC },	  \
  { "link_arch",	 LINK_ARCH_SPEC },
    
/*
 * This should be the same as in sol2.h, except with "/sparcv9"
 * appended to the paths and /usr/ccs/lib is no longer necessary
 */
#define LINK_ARCH64_SPEC \
  "%{mcmodel=medlow:-M /usr/lib/ld/sparcv9/map.below4G} \
   %{G:-G} \
   %{YP,*} \
   %{R*} \
   %{compat-bsd: \
     %{!YP,*:%{p|pg:-Y P,/usr/ucblib/sparcv9:/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{!p:%{!pg:-Y P,/usr/ucblib/sparcv9:/usr/lib/sparcv9}}} \
     -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{p|pg:-Y P,/usr/lib/libp/sparcv9:/usr/lib/sparcv9} \
       %{!p:%{!pg:-Y P,/usr/lib/sparcv9}}}}"

#undef LINK_ARCH_SPEC
#define LINK_ARCH_SPEC "\
%{m32:%(link_arch32)} \
%{m64:%(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"

#define LINK_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? LINK_ARCH32_SPEC : LINK_ARCH64_SPEC)

#undef	CC1_SPEC
#if DEFAULT_ARCH32_P
#define CC1_SPEC "\
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
%{m32:%{m64:%emay not use both -m32 and -m64}} \
%{m64:-mptr64 -mstack-bias -mno-v8plus \
  %{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8*:%{!msupersparc:-mcpu=v9}}}}}}}} \
"
#else
#define CC1_SPEC "\
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
%{m32:%{m64:%emay not use both -m32 and -m64}} \
%{m32:-mptr32 -mno-stack-bias \
  %{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8*:%{!msupersparc:-mcpu=cypress}}}}}}}} \
%{mv8plus:-m32 -mptr32 -mno-stack-bias \
  %{!mcpu*:%{!mcypress:%{!msparclite:%{!mf930:%{!mf934:%{!mv8:%{!msupersparc:-mcpu=v9}}}}}}}} \
"
#endif

#if DEFAULT_ARCH32_P
#define MULTILIB_DEFAULTS { "m32" }
#else
#define MULTILIB_DEFAULTS { "m64" }
#endif

/* We use stabs-in-elf in 32-bit mode, because that is what the native
   toolchain uses.  But gdb can't handle truncated 32-bit stabs so we
   use dwarf2 in 64-bit mode.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE (TARGET_ARCH32 ? DBX_DEBUG : DWARF2_DEBUG)

/* We can't use the above definition for the purposes of specs.  */
#if defined(HAVE_AS_GDWARF2_DEBUG_FLAG) && defined(HAVE_AS_GSTABS_DEBUG_FLAG)
# if DEFAULT_ARCH32_P
#  define ASM_DEBUG_SPEC "%{gdwarf-2*:--gdwarf2}%{!gdwarf-2*:%{g*:--gstabs}}"
# else
#  define ASM_DEBUG_SPEC "%{gstabs*:--gstabs}%{!gstabs*:%{g*:--gdwarf2}}"
# endif
#endif
