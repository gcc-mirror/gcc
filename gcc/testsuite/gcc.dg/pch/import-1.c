/* { dg-options "-I. -I $srcdir/gcc.dg/pch" } */
#include "import-1.h"
#include "import-1a.h"
#import "import-1b.h"
#include "import-1c.h"

#ifndef IMPORT_1A
IMPORT_1A not defined
#endif

#ifndef IMPORT_1B
IMPORT_1B not defined
#endif

#ifndef IMPORT_1C
IMPORT_1C not defined
#endif

#ifndef IMPORT_1
IMPORT_1 not defined
#endif
