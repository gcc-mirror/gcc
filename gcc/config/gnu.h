/* Configuration common to all targets running the GNU system.  */

/* Provide GCC options for standard feature-test macros.  */
#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{bsd:-D_BSD_SOURCE}"

/* Provide an ASM_SPEC appropriate for GNU.  Currently we only deal
   with the options for generating PIC code.  */
#undef ASM_SPEC
#define ASM_SPEC " %| %{fpic:-k} %{fPIC:-k -K}"

/* Default C library spec.  Use -lbsd-compat for gcc -bsd.  */
#undef LIB_SPEC
#define LIB_SPEC "%{bsd:-lbsd-compat} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"


/* We have atexit).  */
#define HAVE_ATEXIT

/* Implicit library calls should use memcpy, not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS
