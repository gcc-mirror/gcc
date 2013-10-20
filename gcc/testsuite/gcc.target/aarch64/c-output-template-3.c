/* { dg-do compile } */

struct tracepoint {
    int dummy;
    int state;
};
static struct tracepoint tp;

void
test (void)
{
    __asm__ ("@ %c0" : : "i" (&tp.state));
}

/* { dg-final { scan-assembler "@ tp\\+4" } } */
