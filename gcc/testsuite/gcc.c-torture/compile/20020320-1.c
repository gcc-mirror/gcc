/* PR bootstrap/4192
   This testcase caused infinite loop in flow (several places),
   because flow assumes gen_jump generates simple_jump_p.  */

typedef void (*T) (void);
extern T x[];

void
foo (void)
{
  static T *p = x;
  static _Bool a;
  T f;

  if (__builtin_expect (a, 0))
    return;

  while ((f = *p))
    {
      p++;
      f ();
    }
  a = 1;
}
