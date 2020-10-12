#include <stdio.h>

extern int num_calls;
int side_effect_c()
{
  num_calls ++;
}
