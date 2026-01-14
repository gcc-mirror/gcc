/* Testing the attribute counted_by for anonymous structures as the top-level
   type and as the type for an unnamed field.  */ 
/* { dg-do run } */
/* { dg-options "-O2 -fsanitize=bounds" } */
/* { dg-output "index 11 out of bounds for type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*index 22 out of bounds for type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */


struct { int a; char b[] __attribute__ ((counted_by (a))); } *x;
struct s { struct { int a; char b[] __attribute__ ((counted_by (a))); } *x; } *y;

int main ()
{
  x = (typeof (x)) __builtin_malloc (sizeof (*x) + sizeof (char) * 10);
  x->a = 10;
  x->b[11] = 0;

  y = (struct s *) __builtin_malloc (sizeof (struct s));
  y->x = (typeof (y->x)) __builtin_malloc (sizeof (y->x) + sizeof (char) * 20);
  y->x->a = 20;
  y->x->b[22] = 0;
  return 0;
}
