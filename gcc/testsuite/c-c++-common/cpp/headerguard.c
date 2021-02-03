/* { dg-do preprocess }  */
/* Check header guards are optimized.  */

#include "noheaderguard-b.h"
#include "noheaderguard-b.h"
#include "noheaderguard-a.h"

/* { dg-final { scan-file headerguard.i {# [0-9]* "[^\n]*headerguard.c"\n\n*# [0-9]* "[^\n]*headerguard-b.h" 1\n\n*# [0-9]* "[^\n]*headerguard-a.h" 1\n\n*# [0-9]* "[^\n]*headerguard-b.h" 2\n\n*# [0-9]* "[^\n]*headerguard.c" 2\n} } } */
