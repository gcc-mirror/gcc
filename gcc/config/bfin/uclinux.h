#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "crt1%O%s crti%O%s crtbegin%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "crtend%O%s crtn%O%s"

#undef  LIB_SPEC
#define LIB_SPEC "%{pthread:-lpthread} -lc"

#define NO_IMPLICIT_EXTERN_C
