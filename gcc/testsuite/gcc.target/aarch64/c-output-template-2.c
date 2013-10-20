/* { dg-do compile } */

struct tracepoint {
    int dummy;
    int state;
};
static struct tracepoint tp;

void
test (void)
{
    __asm__ ("@ %c0" : : "i" (&tp));
}

/* { dg-final { scan-assembler "@ tp" } } */
