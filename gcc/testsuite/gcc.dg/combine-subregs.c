/* { dg-do run { target { stdint_types } } } */
/* { dg-options "-O2" } */

#include <stdint.h>
#include <stdlib.h>

void __attribute__ ((noinline))
foo (uint64_t state, uint32_t last)
{
  if (state == last) abort ();
}

/* This function may do a bad comparision by trying to
   use SUBREGS during the compare on machines where comparing
   two registers always compares the entire register regardless
   of mode.  */

int __attribute__ ((noinline))
compare (uint64_t state, uint32_t *last, uint8_t buf)
{
    if (*last == ((state | buf) & 0xFFFFFFFF)) {
	foo (state, *last);
        return 0;
    }
    return 1;
}

int
main(int argc, char **argv) {
    uint64_t state = 0xF00000100U;
    uint32_t last  = 0x101U;
    int ret        = compare(state, &last, 0x01);
    if (ret != 0)
	abort ();
    exit (0);
}
