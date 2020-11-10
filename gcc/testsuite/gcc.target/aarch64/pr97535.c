/* { dg-do compile { target { ! ilp32 } } } */

#include <string.h>

#define SIZE 2181038080

extern char raw_buffer[SIZE];

void setRaw(const void *raw)
{
        memcpy(raw_buffer, raw, SIZE);
}

/* At any optimization level this should be a function call
   and not inlined.  */
/* { dg-final { scan-assembler "bl\tmemcpy" } } */
