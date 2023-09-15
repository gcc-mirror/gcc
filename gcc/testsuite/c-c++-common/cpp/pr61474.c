/* { dg-do preprocess } */
#include "pr61474.h"
/* Make sure that the file can be included for real, after it was
   fake-included by the linemarker directives in pr61474.h.  */
#include "pr61474-2.h"
