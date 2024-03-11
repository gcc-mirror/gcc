/* { dg-do compile { target { ! ia32  }  }  } */
/* { dg-options "-musermsr -O2"  } */
/* { dg-final { scan-assembler-times "urdmsr\[ \\t\]\\%r\[a-z\]x, \\%r\[a-z\]x" 1  }  } */
/* { dg-final { scan-assembler-times "urdmsr\[ \\t\]\\\$121" 1  }  } */
/* { dg-final { scan-assembler-times "uwrmsr\[ \\t\]\\%r\[a-z\]x, \\%r\[a-z\]x" 1  }  } */
/* { dg-final { scan-assembler-times "uwrmsr\[ \\t\]\\%r\[a-z\]x, \\\$121" 1  }  } */

#include <x86gprintrin.h>

volatile unsigned long long x;
volatile unsigned long long y;

void extern
user_msr_test (void)
{
  x = _urdmsr(y);
  x = _urdmsr(121);
  _uwrmsr(y, x);
  _uwrmsr(121, x);
}
