/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

typedef int v4si __attribute__ ((vector_size (16)));
struct A { int a; int b; };

void foo (int b);

void
fun (struct A a) {

        foo (a.b);
} /* { dg-bogus "the vector type" } */

void
bar ()
{
  struct A a;
  fun (a);
}
