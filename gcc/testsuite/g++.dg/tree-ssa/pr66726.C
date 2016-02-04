/* { dg-do run } */
/* { dg-options "-O2" } */

/* Execution test for converting VIEW_CONVERT_EXPR.  */

struct cpp_num {
  bool f;
};

extern cpp_num  __attribute__((noinline))
foo (cpp_num lhs,
     cpp_num rhs)
{
  lhs.f = lhs.f || rhs.f;
  return lhs;
}

cpp_num lhs, rhs, r;

int main ()
{

  lhs.f = false;
  rhs.f = false;
  r = foo (lhs, rhs);
  if (r.f)
    __builtin_abort ();


  lhs.f = false;
  rhs.f = true;
  r = foo (lhs, rhs);
  if (!r.f)
    __builtin_abort ();
}
