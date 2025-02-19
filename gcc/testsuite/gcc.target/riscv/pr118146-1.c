/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d  -O" { target { rv64 } } } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O" { target { rv32 } } } */



typedef __attribute__((__vector_size__(sizeof(_Float16)))) short V;
_Float16 f;

void
foo(V v)
{
  f -= *(_Float16 *)&v;
}
