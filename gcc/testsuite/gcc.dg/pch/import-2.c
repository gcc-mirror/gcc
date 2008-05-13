/* { dg-options "-I. -I $srcdir/gcc.dg/pch/include -Wno-deprecated" } */

#include "import-2.h"
#import "import-2b.h"

int main(int argc, char **argv) {
   return 0;
}
