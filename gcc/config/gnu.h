/* Configuration common to all targets running the GNU system.  */

/* Provide GCC options for standard feature-test macros.  */
#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{bsd:-D_BSD_SOURCE}"

/* Default C library spec.  Use -lbsd-compat for gcc -bsd.  */
#undef LIB_SPEC
#define LIB_SPEC "%{bsd:-lbsd-compat} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

/* Standard include directory.  In GNU, "/usr" is a four-letter word.  */
#undef STANDARD_INCLUDE_DIR
#define STANDARD_INCLUDE_DIR "/include"


/* We have atexit.  */
#define HAVE_ATEXIT

/* Implicit library calls should use memcpy, not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

/* The system headers under GNU are C++-aware.  */
#define NO_IMPLICIT_EXTERN_C
