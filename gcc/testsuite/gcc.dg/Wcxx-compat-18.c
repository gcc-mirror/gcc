/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */
enum E1 { A };
enum E2 { B };
int
f1 (int i)
{
  return (int) (i ? A : B);	/* { dg-warning "invalid in C\[+\]\[+\]" } */
}
extern enum E1 f2();
int
f3 (int i)
{
  return (int) (i ? f2 () : B);	/* { dg-warning "invalid in C\[+\]\[+\]" } */
}
