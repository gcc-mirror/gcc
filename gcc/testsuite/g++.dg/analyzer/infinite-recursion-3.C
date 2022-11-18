/* Adapted from g++.dg/warn/Winfinite-recursion-3.C  */

typedef __SIZE_TYPE__ size_t;

/* Might throw.  */
void f ();

void warn_f_call_r (int  n)
{
  if (n > 7)
    f ();
  warn_f_call_r (n - 1);
}

void warn_f_do_while_call_r (int n)
{
  f ();
  do
    {
      f ();
      warn_f_do_while_call_r (n - 1);
    }
  while (1);
}


struct X
{
  X (int);
  ~X ();
};

int warn_class_with_ctor (int n)
{
  X x (n);
  return n + warn_class_with_ctor (n - 1);
}


int nowarn_throw (int n)
{
  if (n > 7)
    throw "argument too big";

  return n + nowarn_throw (n - 1);
}

extern int* eipa[];

void warn_call_new (int i)
{
  eipa[i] = new int;

  warn_call_new (i - 1);
}

void* operator new[] (size_t n)
{
  char *p = new char[n + sizeof (n)];
  *(size_t*)p = n;
  return p + sizeof n;
}
