# include "gcconfig.h"
# include <stdio.h>

int main()
{
#   if defined(LINUX_THREADS)
#     ifdef USE_LD_WRAP
	printf("-Wl,--wrap -Wl,read -Wl,--wrap -Wl,dlopen "
	       "-Wl,--wrap -Wl,pthread_create -Wl,--wrap -Wl,pthread_join "
	       "-Wl,--wrap -Wl,pthread_sigmask -lpthread -ldl\n");
#     else
	printf("-lpthread -ldl\n");
#     endif
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
    return 0;
}

