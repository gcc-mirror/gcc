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
#   if defined(LINUX_THREADS)
      printf("-lpthread\n");
#   endif
#   if defined(IRIX_THREADS)
	printf("-lpthread\n");
#   endif
#   if defined(HPUX_THREADS)
	printf("-lpthread -lrt\n");
#   endif
#   ifdef SOLARIS_THREADS
        printf("-lthread -ldl\n");
#   endif
#   ifdef GC_OSF1_THREADS
	printf("-lpthread -lrt\n");
#   endif
    return 0;
}

