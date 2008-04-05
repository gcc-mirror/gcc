/* { dg-do run } */
/* { dg-options "-ftree-vrp" } */

struct S {struct S *field;};
struct S True, False, Z;
static inline int f(void) {return 1;}
static inline int g(struct S **obj) {
   return f() && *obj == &Z;
}
struct S **h(struct S **x) {
   if (x)
      return g(x) ? &True.field : &False.field;
   else
      return &True.field;
}
extern void abort (void);
int main()
{
  if (h(&False.field) != &False.field)
    abort ();
  return 0;
}
