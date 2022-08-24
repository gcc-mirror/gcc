/* { dg-do compile } */

typedef int __attribute__((vector_size(16))) vec_t;

vec_t src, inv, res;

void test(int i)
{
    vec_t y={0};
    y[i] = (i & 1 ? inv : src)[i];
    res = y;
}
