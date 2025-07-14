/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -march=x86-64 -fpic" } */

typedef struct {
  struct {
    unsigned short lo4;
    unsigned short lo3;
    unsigned short lo2;
    unsigned short lo1;
  } i;
} BID_BINARY80LDOUBLE;
extern BID_BINARY80LDOUBLE __bid64_to_binary80_x_out;
void
__bid64_to_binary80 (void)
{
  __bid64_to_binary80_x_out.i.lo4
    = __bid64_to_binary80_x_out.i.lo3
    = __bid64_to_binary80_x_out.i.lo2
    = __bid64_to_binary80_x_out.i.lo1 = 65535;
}

/* { dg-final { scan-assembler-times "movq\[ \\t\]+%xmm\[0-9\]+, " 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movq\[ \\t\]+\\\$-1, \\(%(e|r)\[a-z0-9\]+\\)" 1 { target { ! ia32 } } } } */
