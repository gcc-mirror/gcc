/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mregparm=3" } */
/* { dg-final { scan-assembler-not {call[ \t]+_?bar} } } */

#include <stdarg.h>

void (*bar)(int, va_list); 

void foo(int i, va_list args)
{
  bar(i, args);
}
