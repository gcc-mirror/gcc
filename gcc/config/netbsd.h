/* NETBSD_NATIVE is defined when gcc is integrated into the NetBSD
   source tree so it can be configured appropriately without using
   the GNU configure/build mechanism. */

#ifdef NETBSD_NATIVE

/* Look for the include files in the system-defined places.  */

#undef GPLUSPLUS_INCLUDE_DIR
#define GPLUSPLUS_INCLUDE_DIR "/usr/include/g++"

#undef GCC_INCLUDE_DIR
#define GCC_INCLUDE_DIR "/usr/include"

#undef INCLUDE_DEFAULTS
#define INCLUDE_DEFAULTS			\
  {						\
    { GPLUSPLUS_INCLUDE_DIR, "G++", 1, 1 },	\
    { GCC_INCLUDE_DIR, "GCC", 0, 0 },		\
    { 0, 0, 0, 0 }				\
  }

/* Under NetBSD, the normal location of the compiler back ends is the
   /usr/libexec directory.  */

#undef STANDARD_EXEC_PREFIX
#define STANDARD_EXEC_PREFIX		"/usr/libexec/"

/* Under NetBSD, the normal location of the various *crt*.o files is the
   /usr/lib directory.  */

#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX	"/usr/lib/"

#endif /* NETBSD_NATIVE */


/* Provide a CPP_SPEC appropriate for NetBSD.  Currently we just deal with
   the GCC option `-posix'.  */

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %{posix:-D_POSIX_SOURCE}"


/* Provide a LIB_SPEC appropriate for NetBSD.  Just select the appropriate
   libc, depending on whether we're doing profiling; if `-posix' is specified,
   link against the appropriate libposix first.  Don't include libc when
   linking a shared library.  */

#undef LIB_SPEC
#define LIB_SPEC		\
  "%{posix:			\
     %{!p:			\
       %{!pg:-lposix}}		\
     %{p:-lposix_p}		\
     %{pg:-lposix_p}}		\
   %{!shared:			\
     %{!symbolic:		\
       %{!p:			\
	 %{!pg:-lc}}		\
       %{p:-lc_p}		\
       %{pg:-lc_p}}}"

/* Provide a LIBGCC_SPEC appropriate for NetBSD.  We also want to exclude
   libgcc with -symbolic.  */

#undef LIBGCC_SPEC
#ifdef NETBSD_NATIVE
#define LIBGCC_SPEC		\
  "%{!symbolic:			\
     %{!shared:			\
       %{!p:			\
	 %{!pg: -lgcc}}}		\
     %{shared: -lgcc_pic}	\
     %{p: -lgcc_p}		\
     %{pg: -lgcc_p}}"
#else
#define LIBGCC_SPEC "%{!shared:%{!symbolic: -lgcc}}"
#endif

/* When building shared libraries, the initialization and finalization 
   functions for the library are .init and .fini respectively.  */

#define COLLECT_SHARED_INIT_FUNC(STREAM,FUNC)				\
  do {									\
    fprintf ((STREAM), "void __init() __asm__ (\".init\");");		\
    fprintf ((STREAM), "void __init() {\n\t%s();\n}\n", (FUNC));	\
  } while (0)

#define COLLECT_SHARED_FINI_FUNC(STREAM,FUNC)				\
  do {									\
    fprintf ((STREAM), "void __fini() __asm__ (\".fini\");");		\
    fprintf ((STREAM), "void __fini() {\n\t%s();\n}\n", (FUNC));	\
  } while (0)

/* Allow #sccs in preprocessor.  */

#undef SCCS_DIRECTIVE
#define SCCS_DIRECTIVE

#undef TARGET_HAS_F_SETLKW
#define TARGET_HAS_F_SETLKW

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#undef TARGET_MEM_FUNCTIONS
#define TARGET_MEM_FUNCTIONS 1

/* Handle #pragma weak and #pragma pack.  */

#define HANDLE_SYSV_PRAGMA
