/* { dg-options "-std=c99 -Wc++-compat -Werror" { target c } } */
/* { dg-prune-output "treated as errors" } */
#include <stdio.h>

int main()
{
  for (int *p = (int[]){ 1, 2, 3, 0 }; /* { dg-error "array" } */
       *p; ++p) {
    printf("%d\n", *p);
  }
  return 0;
}
