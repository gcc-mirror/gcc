/* PR debug/69518 */
/* { dg-do compile } */
/* { dg-options "-g" } */

struct A a;
typedef struct A B;
struct A {}
foo (B x)
{
  __builtin_abort ();
}
