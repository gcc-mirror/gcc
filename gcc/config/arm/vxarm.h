#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC      "%{march=arm710:-DCPU=ARM710A} \
%{march=arm7tdmi:-DCPU=ARM7TDMI} \
%{march=arm810:-DCPU=ARM810} \
%{march=strongarm110:-DCPU=ARMSA110} \
%{!march=*: \
 %{mcpu=arm710:-DCPU=ARM710A} \
 %{mcpu=arm7tdmi:-DCPU=ARM7TDMI} \
 %{mcpu=arm810:-DCPU=ARM810} \
 %{mcpu=strongarm110:-DCPU=ARMSA110}} \
%{!mcpu*:%{!march=*:-DCPU=ARM710A}} \
"

#define SUBTARGET_CPU_DEFAULT TARGET_CPU_arm710

#undef CPP_PREDEFINES
#define CPP_PREDEFINES  "-D__vxworks -D__arm__ -Acpu(arm) -Amachine(arm)"

/* VxWorks does all the library stuff itself.  */

#undef LIB_SPEC
#define LIB_SPEC ""

/* VxWorks uses object files, not loadable images.  make linker just
   combine objects. */

#undef LINK_SPEC
#define LINK_SPEC "-r"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""
