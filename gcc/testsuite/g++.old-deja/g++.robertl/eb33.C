#include <fstream.h>
#include <stdio.h>

int
main()
{
    printf("If you see this, you don't have a problem!\n");
#ifdef EXPOSE_BUG
    ifstream a;
#endif
}
