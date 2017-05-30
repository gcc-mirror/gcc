/* PR sanitizer/66343 */
/* { dg-options "-fsanitize=undefined" } */

#include "pr66343-2.h"

void
bar (int a, int b)
{
  a / b;
}
