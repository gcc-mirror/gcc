/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -musermsr" } */
/* { dg-final { scan-assembler-times "urdmsr\[ \\t\]\\%r\[a-z\]x, \\%r\[a-z\]x" 1  }  } */
/* { dg-final { scan-assembler-times "uwrmsr\[ \\t\]\\%r\[a-z\]x, \\%r\[a-z\]x" 1  }  } */
/* { dg-final { scan-assembler-times "movabsq\[ \\t\]\\\$20018842566655, \\%r\[a-z\]x" 1  }  } */

#include <x86gprintrin.h>

volatile unsigned long long x;

void extern
user_msr_test (void)
{
  x = _urdmsr(0x1234ffffffffULL);
  _uwrmsr(0x1234ffffffffULL, x);
}
