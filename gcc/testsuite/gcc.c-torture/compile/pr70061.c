/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

typedef int v8si __attribute__ ((vector_size (32)));

int
foo(v8si c, v8si d)
{
l0:
  if (c[2])
    d ^= c;
  return d[3];
}
