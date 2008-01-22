#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "\
%{msim:%{!shared:crt0%O%s}} \
%{!msim:basiccrt%O%s} \
crti%O%s crtbegin%O%s crtlibid%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC	"crtend%O%s crtn%O%s"

#undef  LIB_SPEC
#define LIB_SPEC "--start-group -lc %{msim:-lsim}%{!msim:-lnosys} --end-group \
%{!T*:%{!msim:%{mcpu=bf522*:-T bf522.ld%s}%{mcpu=bf523*:-T bf523.ld%s} \
	      %{mcpu=bf524*:-T bf524.ld%s}%{mcpu=bf525*:-T bf525.ld%s} \
	      %{mcpu=bf526*:-T bf526.ld%s}%{mcpu=bf527*:-T bf527.ld%s} \
	      %{mcpu=bf531*:-T bf531.ld%s}%{mcpu=bf532*:-T bf532.ld%s} \
	      %{mcpu=bf533*:-T bf533.ld%s}%{mcpu=bf534*:-T bf534.ld%s} \
	      %{mcpu=bf536*:-T bf536.ld%s}%{mcpu=bf537*:-T bf537.ld%s} \
	      %{mcpu=bf538*:-T bf538.ld%s}%{mcpu=bf539*:-T bf539.ld%s} \
	      %{mcpu=bf542*:-T bf542.ld%s}%{mcpu=bf544*:-T bf544.ld%s} \
	      %{mcpu=bf547*:-T bf547.ld%s}%{mcpu=bf548*:-T bf548.ld%s} \
	      %{mcpu=bf549*:-T bf549.ld%s} \
	      %{!mcpu=*:-T bf532.ld%s} \
	      -T bfin-common-sc.ld%s}}"

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#ifdef __BFIN_FDPIC__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
asm (SECTION_OP); \
asm ("P3 = [SP + 20];\n\tcall " USER_LABEL_PREFIX #FUNC ";"); \
asm (TEXT_SECTION_ASM_OP);
#endif

#undef SUBTARGET_DRIVER_SELF_SPECS
#define SUBTARGET_DRIVER_SELF_SPECS \
     "%{mfdpic:-msim} %{mid-shared-library:-msim}"

#define NO_IMPLICIT_EXTERN_C
