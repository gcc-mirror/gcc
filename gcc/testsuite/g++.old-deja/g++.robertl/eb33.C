// { dg-do run  }
#include <fstream>
#include <cstdio>

int
main()
{
    std::printf("If you see this, you don't have a problem!\n");
#ifdef EXPOSE_BUG
    std::ifstream a;
#endif
}
