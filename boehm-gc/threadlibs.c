# include "gcconfig.h"
# include <stdio.h>

int main()
{
#   if defined(LINUX_THREADS)
#     ifdef USE_LD_WRAP
	printf("-Wl,\"--wrap read\" -Wl,\"--wrap dlopen\" "
	       "-Wl,\"--wrap pthread_create\" -Wl,\"--wrap pthread_join\" "
	       "-Wl,\"--wrap pthread_sigmask\" -lpthread\n");
#     else
	printf("-lpthread\n");
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

