/* Define if you can safely include both <string.h> and <strings.h>.  */
#undef STRING_WITH_STRINGS

/* Define if printf supports "%p".  */
#undef HAVE_PRINTF_PTR

/* Define if you want to always select the new-abi for g++.  */
#undef ENABLE_NEW_GXX_ABI

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

/* Define to 1 if installation paths should be looked up in Windows32
   Registry. Ignored on non windows32 hosts. */
#undef ENABLE_WIN32_REGISTRY

/* Define to be the last portion of registry key on windows hosts.  */
#undef WIN32_REGISTRY_KEY

/* Define as 1 if you have catgets and don't want to use GNU gettext.  */
#undef HAVE_CATGETS

/* Define as 1 if you have gettext and don't want to use GNU gettext.  */
#undef HAVE_GETTEXT

/* Define if your compiler understands volatile.  */
#undef HAVE_VOLATILE

/* Define if your assembler supports specifying the maximum number
   of bytes to skip when using the GAS .p2align command. */
#undef HAVE_GAS_MAX_SKIP_P2ALIGN

/* Define if your assembler supports .balign and .p2align.  */
#undef HAVE_GAS_BALIGN_AND_P2ALIGN

/* Define if the assembler supports 64bit sparc.  */
#undef AS_SPARC64_FLAG

/* Define if your assembler supports offsetable %lo().  */
#undef HAVE_AS_OFFSETABLE_LO10

/* Define if your assembler supports .register.  */
#undef HAVE_AS_REGISTER_PSEUDO_OP

/* Define if your assembler supports .subsection and .subsection -1 starts
   emitting at the beginning of your section */
#undef HAVE_GAS_SUBSECTION_ORDERING

/* Define if your assembler supports .weak.  */
#undef HAVE_GAS_WEAK

/* Define if your assembler supports .hidden.  */
#undef HAVE_GAS_HIDDEN

/* Define if your assembler uses the old HImode fild and fist notation.  */
#undef HAVE_GAS_FILDS_FISTS

/* Define if you have a working <inttypes.h> header file.  */
#undef HAVE_INTTYPES_H

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

/* Define to enable the use of a default assembler. */
#undef DEFAULT_ASSEMBLER

/* Define to enable the use of a default linker. */
#undef DEFAULT_LINKER

/* Define if host mkdir takes a single argument. */
#undef MKDIR_TAKES_ONE_ARG

/* Define to the name of the distribution.  */
#undef PACKAGE

/* Define to the version of the distribution.  */
#undef VERSION
@TOP@
