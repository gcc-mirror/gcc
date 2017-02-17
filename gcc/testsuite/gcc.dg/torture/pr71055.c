/* { dg-do run } */

extern void abort (void);
union U { int i; _Bool b; char c; };
void __attribute__((noinline,noclone))
foo (union U *u)
{
  if (u->c != 0)
    abort ();
}
int main()
{
  union U u;
  u.i = 10;
  u.b = 0;
  foo (&u);
  return 0;
}
