/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

typedef int v4si __attribute__ ((vector_size (16)));
struct A { int a; v4si b; };

void
fun (struct A a) {} /* { dg-bogus "the scalable vector type" } */

void
bar ()
{
  struct A a;
  fun (a);
}
