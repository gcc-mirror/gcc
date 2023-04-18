/* PR target/94908 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

typedef float v4sf __attribute__((vector_size(16)));

v4sf g();

v4sf f(v4sf a, v4sf b)
{
    return (v4sf){g()[1], a[1], a[2], a[3]};
}

/* { dg-final { scan-assembler "\[ \t\]v?insertps" } } */
