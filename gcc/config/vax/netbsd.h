#include <vax/vax.h>
#include <netbsd.h>

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dvax -D__NetBSD__ -Asystem(unix) -Asystem(NetBSD) -Acpu(vax) -Amachine(vax)"
