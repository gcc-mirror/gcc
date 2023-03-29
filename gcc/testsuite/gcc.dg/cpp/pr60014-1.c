/* { dg-do compile } */
/* { dg-options "-save-temps -Wint-conversion" } */
#include "pr60014-1.h"
int main ()
{
    X(a, 
      b);
    char *should_warn = 1; /* { dg-warning {-Wint-conversion} } */
}
