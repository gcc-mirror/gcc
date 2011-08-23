/* Test that we don't generate a fisttp instruction when -mno-sse3.  */
/* { dg-do compile } */
/* { dg-options "-O -mfpmath=387 -march=nocona -mno-sse3 -mno-avx" } */
/* { dg-final { scan-assembler-not "fisttp" } } */
struct foo
{
 long a;
 long b;
};

extern double c;

extern unsigned long long baz (void);

int
walrus (const struct foo *input)
{
    unsigned long long d;

    d = baz ()
      + (unsigned long long) (((double) input->a * 1000000000
			      + (double) input->b) * c);
    return (d ? 1 : 0);
}
