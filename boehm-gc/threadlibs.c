# include "private/gcconfig.h"
# include <stdio.h>

int main()
{
#   if defined(GC_USE_LD_WRAP)
	printf("-Wl,--wrap -Wl,read -Wl,--wrap -Wl,dlopen "
	       "-Wl,--wrap -Wl,pthread_create -Wl,--wrap -Wl,pthread_join "
	       "-Wl,--wrap -Wl,pthread_detach "
	       "-Wl,--wrap -Wl,pthread_sigmask -Wl,--wrap -Wl,sleep\n");
#   endif
#   if defined(GC_LINUX_THREADS) || defined(GC_IRIX_THREADS) \
	|| defined(GC_FREEBSD_THREADS) || defined(GC_SOLARIS_PTHREADS)
        printf("-lpthread\n");
#   endif
#   if defined(GC_HPUX_THREADS) || defined(GC_OSF1_THREADS)
	printf("-lpthread -lrt\n");
#   endif
#   if defined(GC_SOLARIS_THREADS) && !defined(GC_SOLARIS_PTHREADS)
        printf("-lthread -ldl\n");
#   endif
    return 0;
}

