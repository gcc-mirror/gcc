/* { dg-do compile } */
/* { dg-options "-save-temps -fpermissive -Wint-conversion" } */
#include "pr60014-2.h"
X
char *should_warn = 1; /* { dg-warning {-Wint-conversion} } */
