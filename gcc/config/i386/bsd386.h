/* Configuration for an i386 running BSDI's BSD/386 1.1 as the target
   machine.  */

#include "i386/386bsd.h"

/* We exist mostly to add -Dbsdi and such to the predefines. */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -Dbsdi -D__i386__ -D__bsdi__ -D____386BSD____ -D__386BSD__ -DBSD_NET2 -Asystem(unix) -Asystem(bsd) -Acpu(i386) -Amachine(i386)"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32
