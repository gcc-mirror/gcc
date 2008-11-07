/* { dg-do link } */
/* { dg-options "-O" } */

/* When optimized we expect the call to foo () in bar to be inlined
   and the call to link_error optimized away.  */

extern void link_error (void);
inline int __attribute__((always_inline)) foo(void) { return 0; }

int main()
{
  int (*fn)(void) = foo;
  if (fn())
    link_error ();
  return 0;
}

