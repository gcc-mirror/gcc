/* PR tree-optimization/93100 - gcc -fsanitize=address inhibits -Wuninitialized
   { dg-do compile }
   { dg-options "-Wall -fsanitize=address" }
   { dg-skip-if "no address sanitizer" { no_fsanitize_address } } */

struct A
{
  _Bool b;
  int i;
};

void warn_A_b_O0 (void)
{
  struct A a;

  if (a.b)          // { dg-warning "\\\[-Wuninitialized" }
    {
      (void)&a;
    }
}

void warn_A_i_O0 (void)
{
  struct A a;

  if (a.i)          // { dg-warning "\\\[-Wuninitialized" }
    {
      (void)&a;
    }
}

#pragma GCC optimize ("1")

void warn_A_b_O1 (void)
{
  struct A a;

  if (a.b)          // { dg-warning "\\\[-Wuninitialized" }
    {
      (void)&a;
    }
}

void warn_A_i_O1 (void)
{
  struct A a;

  if (a.i)          // { dg-warning "\\\[-Wuninitialized" }
    {
      (void)&a;
    }
}


#pragma GCC optimize ("2")

void warn_A_b_O2 (void)
{
  struct A a;

  if (a.b)          // { dg-warning "\\\[-Wuninitialized" }
    {
      (void)&a;
    }
}

void warn_A_i_O2 (void)
{
  struct A a;

  if (a.i)          // { dg-warning "\\\[-Wuninitialized" }
    {
      (void)&a;
    }
}
