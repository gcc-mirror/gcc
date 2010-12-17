/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "12345" } } */

static inline unsigned int myrnd (void)
{
    static unsigned int s = 1388815473;
    s *= 1103515245;
    s += 12345;
}

struct __attribute__ ((packed)) A {
    unsigned short i:1, l:1, j:3, k:11;
};

struct A sA;
void testA (void)
{
    char *p = (char *) &sA;
    *p++ = myrnd ();
    *p++ = myrnd ();
    sA.k = -1;
}
