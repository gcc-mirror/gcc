# include "config.h"
# include <stdio.h>

int main()
{
#   if defined(IRIX_THREADS) || defined(LINUX_THREADS)
	printf("-lpthread\n");
#   endif
#   ifdef SOLARIS_THREADS
        printf("-lthread -ldl\n");
#   endif
    return 0;
}

