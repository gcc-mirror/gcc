/* Define if you want to enable namespaces (-fhonor-std) by default. */
#undef ENABLE_STD_NAMESPACE
#if !defined(ENABLE_STD_NAMESPACE)
#  define ENABLE_STD_NAMESPACE 0
#endif

/* Define if you want more run-time sanity checks.  This one gets a grab
   bag of miscellaneous but relatively cheap checks.  */
#undef ENABLE_CHECKING

/* Define if you want all operations on trees (the basic data
   structure of the front ends) to be checked for dynamic type safety
   at runtime.  This is moderately expensive.  */
#undef ENABLE_TREE_CHECKING

/* Define if you want all operations on RTL (the basic data structure
   of the optimizer and back end) to be checked for dynamic type safety
   at runtime.  This is quite expensive.  */
#undef ENABLE_RTL_CHECKING

/* Define if you want the garbage collector to do object poisoning and
   other memory allocation checks.  This is quite expensive.  */
#undef ENABLE_GC_CHECKING

/* Define if you want the garbage collector to operate in maximally
   paranoid mode, validating the entire heap and collecting garbage at
   every opportunity.  This is extremely expensive.  */
#undef ENABLE_GC_ALWAYS_COLLECT

/* Define to 1 if NLS is requested.  */
#undef ENABLE_NLS

/* Define as 1 if you have catgets and don't want to use GNU gettext.  */
#undef HAVE_CATGETS

/* Define as 1 if you have gettext and don't want to use GNU gettext.  */
#undef HAVE_GETTEXT

/* Define if your assembler supports specifying the maximum number
   of bytes to skip when using the GAS .p2align command. */
#undef HAVE_GAS_MAX_SKIP_P2ALIGN

/* Define if your assembler supports .balign and .p2align.  */
#undef HAVE_GAS_BALIGN_AND_P2ALIGN

/* Define if your assembler uses the old HImode fild and fist notation.  */
#undef HAVE_GAS_FILDS_FISTS

/* Define if your locale.h file contains LC_MESSAGES.  */
#undef HAVE_LC_MESSAGES

/* Define as 1 if you have the stpcpy function.  */
#undef HAVE_STPCPY

/* Whether malloc must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_MALLOC

/* Whether realloc must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_REALLOC

/* Whether calloc must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_CALLOC

/* Whether free must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_FREE

/* Whether bcopy must be declared even if <string.h> is included.  */
#undef NEED_DECLARATION_BCOPY

/* Whether bcmp must be declared even if <string.h> is included.  */
#undef NEED_DECLARATION_BCMP

/* Whether bzero must be declared even if <string.h> is included.  */
#undef NEED_DECLARATION_BZERO

/* Whether index must be declared even if <string.h> is included.  */
#undef NEED_DECLARATION_INDEX

/* Whether rindex must be declared even if <string.h> is included.  */
#undef NEED_DECLARATION_RINDEX

/* Whether getenv must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_GETENV

/* Whether atol must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_ATOL

/* Whether atof must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_ATOF

/* Whether sbrk must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_SBRK

/* Whether abort must be declared even if <stdlib.h> is included.  */
#undef NEED_DECLARATION_ABORT

/* Whether strsignal must be declared even if <string.h> is included.  */
#undef NEED_DECLARATION_STRSIGNAL

/* Whether strstr must be declared even if <string.h> is included.  */
#undef NEED_DECLARATION_STRSTR

/* Whether getcwd must be declared even if <unistd.h> is included.  */
#undef NEED_DECLARATION_GETCWD

/* Whether getwd must be declared even if <unistd.h> is included.  */
#undef NEED_DECLARATION_GETWD

/* Whether getrlimit must be declared even if <sys/resource.h> is included.  */
#undef NEED_DECLARATION_GETRLIMIT

/* Whether setrlimit must be declared even if <sys/resource.h> is included.  */
#undef NEED_DECLARATION_SETRLIMIT

/* Whether getrusage must be declared even if <sys/resource.h> is included.  */
#undef NEED_DECLARATION_GETRUSAGE

/* Whether putc_unlocked must be declared even if <stdio.h> is included.  */
#undef NEED_DECLARATION_PUTC_UNLOCKED

/* Whether fputs_unlocked must be declared even if <stdio.h> is included.  */
#undef NEED_DECLARATION_FPUTS_UNLOCKED

/* Whether environ must be declared.  */
#undef NEED_DECLARATION_ENVIRON

@TOP@
