// { dg-do compile }
// { dg-options "-O0" }

typedef int tf ();

struct S
{
  tf m_fn1;
} a;

void
fn1 ()
{
  try
    {
      __builtin_va_list c;
      {
	int *d = __builtin_va_arg (c, int *);
	int **e = &d;
	__asm__("" : "=d"(e));
	a.m_fn1 ();
      }
      a.m_fn1 ();
    }
  catch (...)
    {

    }
}
