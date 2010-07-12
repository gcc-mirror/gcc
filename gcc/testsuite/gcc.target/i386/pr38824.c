/* { dg-do compile } */
/* { dg-options "-O2 -msse" } */
/* { dg-require-effective-target sse } */

typedef float v4sf __attribute__ ((__vector_size__ (16)));

void bench_1(float * out, float * in, float f, unsigned int n)
{
    n /= 4;
    v4sf scalar = { f, f, f, f };
    do
    {
        v4sf arg = *(v4sf *)in;
        v4sf result = arg + scalar;
        *(v4sf *) out = result;
        in += 4;
        out += 4;
    }
    while (--n);
}

/* { dg-final { scan-assembler-not "addps\[^\\n\]*%\[er\]" } } */
