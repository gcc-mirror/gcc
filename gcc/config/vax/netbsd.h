#include <vax/vax.h>
#include <netbsd.h>

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dvax -D__NetBSD__ -Asystem(unix) -Asystem(NetBSD) -Acpu(vax) -Amachine(vax)"

/* Make gcc agree with <machine/ansi.h> */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32
