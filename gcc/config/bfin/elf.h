#define OBJECT_FORMAT_ELF

#define LOCAL_LABEL_PREFIX "L$"

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)		\
     sprintf (LABEL, "*%s%s$%d", LOCAL_LABEL_PREFIX, PREFIX, (int) NUM)

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC	"crt0%O%s crti%O%s crtbegin%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC	"crtend%O%s crtn%O%s"

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"
