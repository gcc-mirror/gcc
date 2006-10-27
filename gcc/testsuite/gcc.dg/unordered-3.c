/* { dg-do link } */
/* { dg-options "-fno-finite-math-only" }  */

void link_error ();

void test1()
{
  if (__builtin_isgreater(1.0,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_isgreater(__builtin_nan(""),1.0) != 0)
    link_error ();

  if (__builtin_isgreaterequal(1.0,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_isgreaterequal(__builtin_nan(""),1.0) != 0)
    link_error ();

  if (__builtin_isless(1.0,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_isless(__builtin_nan(""),1.0) != 0)
    link_error ();

  if (__builtin_islessequal(1.0,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_islessequal(__builtin_nan(""),1.0) != 0)
    link_error ();

  if (__builtin_islessgreater(1.0,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_islessgreater(__builtin_nan(""),1.0) != 0)
    link_error ();

  if (__builtin_isunordered(1.0,__builtin_nan("")) == 0)
    link_error ();
  if (__builtin_isunordered(__builtin_nan(""),1.0) == 0)
    link_error ();
}


void test2(double x)
{
  if (__builtin_isgreater(x,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_isgreater(__builtin_nan(""),x) != 0)
    link_error ();

  if (__builtin_isgreaterequal(x,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_isgreaterequal(__builtin_nan(""),x) != 0)
    link_error ();

  if (__builtin_isless(x,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_isless(__builtin_nan(""),x) != 0)
    link_error ();

  if (__builtin_islessequal(x,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_islessequal(__builtin_nan(""),x) != 0)
    link_error ();

  if (__builtin_islessgreater(x,__builtin_nan("")) != 0)
    link_error ();
  if (__builtin_islessgreater(__builtin_nan(""),x) != 0)
    link_error ();

  if (__builtin_isunordered(x,__builtin_nan("")) == 0)
    link_error ();
  if (__builtin_isunordered(__builtin_nan(""),x) == 0)
    link_error ();
}


int main()
{
  test1 ();
  test2 (1.0);
  return 0;
}

