#undef LIB_SPEC
#define LIB_SPEC	"%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"
 
#undef STARTFILE_SPEC
#define STARTFILE_SPEC	"%{pg:gcrt0.o%s}%{!pg:%{p:gcrt0.o%s}%{!p:crt0.o%s}}"
