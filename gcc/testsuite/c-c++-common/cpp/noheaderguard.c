/* { dg-do preprocess }  */
/* { dg-additional-options -fno-header-guard-opt }  */
/* Check header guard optimization is disabled.  */

#include "noheaderguard-b.h"
#include "noheaderguard-b.h"
#include "noheaderguard-a.h"

/* { dg-final { scan-file noheaderguard.i {# [0-9]* "[^\n]*noheaderguard.c"\n\n*# [0-9]* "[^\n]*noheaderguard-b.h" 1\n\n*# [0-9]* "[^\n]*noheaderguard-a.h" 1\n\n*# [0-9]* "[^\n]*noheaderguard-b.h" 2\n\n*# [0-9]* "[^\n]*noheaderguard.c" 2\n\n*# [0-9]* "[^\n]*noheaderguard-b.h" 1\n\n*# [0-9]* "[^\n]*noheaderguard.c" 2\n\n*# [0-9]* "[^\n]*noheaderguard-a.h" 1\n\n*# [0-9]* "[^\n]*noheaderguard.c" 2\n} } } */
