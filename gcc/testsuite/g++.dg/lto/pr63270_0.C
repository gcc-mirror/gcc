// { dg-lto-do link }
// { dg-lto-options {{-flto -O2 -Wno-odr}} }

#include "pr63270.h"

int main()
{
    return 0;
}
