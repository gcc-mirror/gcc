#define OBJECT_FORMAT_ELF

#define LOCAL_LABEL_PREFIX "L$"

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)		\
     sprintf (LABEL, "*%s%s$%d", LOCAL_LABEL_PREFIX, PREFIX, (int) NUM)

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "\
%{msim:%{!shared:crt0%O%s}} \
%{!msim:%{mcpu=bf531|mcpu=bf532|mcpu=bf533 \
	  |mcpu=bf534|mcpu=bf536|mcpu=bf537:crt532%O%s} \
	%{!mcpu=*:crt532%O%s}} \
crti%O%s crtbegin%O%s crtlibid%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC	"crtend%O%s crtn%O%s"

#undef  LIB_SPEC
#define LIB_SPEC "--start-group -lc %{msim:-lsim}%{!msim:-lnosys} --end-group \
%{!T*:%{!msim:%{mcpu=bf531:-Tbf531.ld}%{mcpu=bf532:-Tbf532.ld} \
	      %{mcpu=bf533:-Tbf533.ld}%{mcpu=bf534:-Tbf534.ld} \
	      %{mcpu=bf536:-Tbf536.ld}%{mcpu=bf537:-Tbf537.ld} \
	      %{!mcpu=*:-Tbf532.ld}}}"

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#ifdef __BFIN_FDPIC__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
asm (SECTION_OP); \
asm ("P3 = [SP + 20];\n\tcall " USER_LABEL_PREFIX #FUNC ";"); \
asm (TEXT_SECTION_ASM_OP);
#endif

#define NO_IMPLICIT_EXTERN_C
