// Direct stores through reference and this parameters can throw non-call
// exceptions.

// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fdump-rtl-expand" }

void
store_ref (int &value)
{
  try
    {
      value = 1;
    }
  catch (...)
    {
      __builtin_trap ();
    }
}

struct S
{
  int value;
  void store ();
};

void
S::store ()
{
  try
    {
      value = 1;
    }
  catch (...)
    {
      __builtin_trap ();
    }
}

// { dg-final { scan-rtl-dump-times {REG_EH_REGION \(const_int [1-9][0-9]*} 2 "expand" } }
