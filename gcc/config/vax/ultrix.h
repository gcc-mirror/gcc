#undef CPP_PREDEFINES
#define CPP_PREDEFINES " -Dvax -Dunix -Dultrix -Dbsd4_2 -D__vax -D__unix -D__ultrix -D__bsd4_2 -Asystem(unix) -Asystem(bsd) -Acpu(vax) -Amachine(vax)"

/* These are as defined in /usr/include/sys/stdtypes.h.
   These values are for ultrix 4.2 on the vax.  */
#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"
#define WCHAR_TYPE "unsigned int"
#define WCHAR_TYPE_SIZE 32

/* True for Ultrix 4.3 and later and possibly earlier.  */
#define HAVE_ATEXIT
