/* PR middle-end/88232 - Please implement -Winfinite-recursion
   { dg-do compile }
   Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.
   { dg-options "-Wall -Winfinite-recursion" } */

typedef __SIZE_TYPE__ size_t;

/* Might throw.  */
void f ();

/* Verify a warning is issued even though a call to f() might throw,
   breaking the infinite recursion.  */

void warn_f_call_r (int  n)   // { dg-warning "-Winfinite-recursion" }
{
  if (n > 7)
    f ();
  warn_f_call_r (n - 1);      // { dg-message "recursive call" }
}

void warn_f_do_while_call_r (int n)    // { dg-warning "-Winfinite-recursion" }
{
  f ();
  do
    {
      f ();
      warn_f_do_while_call_r (n - 1);  // { dg-message "recursive call" }
    }
  while (1);
}


struct X
{
  X (int);
  ~X ();
};

/* Verify a warning even though the X ctor might throw, breaking
   the recursion.  Using possible throwing to suppress the warning
   would make it pretty much useless in C++.  */

int warn_class_with_ctor (int n)    // { dg-warning "-Winfinite-recursion" }
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


/* Verify call operator new doesn't suppress the warning even though
   it might throw.  */

extern int* eipa[];

void warn_call_new (int i)          // { dg-warning "-Winfinite-recursion" }
{
  eipa[i] = new int;

  warn_call_new (i - 1);
}

/* Verify a recursive call to operator new.  */

void* operator new[] (size_t n)     // { dg-warning "-Winfinite-recursion" }
{
  char *p = new char[n + sizeof (n)];   // { dg-message "recursive call" }
  *(size_t*)p = n;
  return p + sizeof n;
}
