#define DEFINED_FUNC_2(x) (3 + (x))

#include "macro-3.h"

int main(void) 
{
  return DEFINED_FUNC (1) - DEFINED_FUNC_2 (-1);
}
