/* { dg-do compile } */
/* { dg-options "-O3" } */

class test
{
public:
  test (int val, int *p)
  {
    int_val = *p;
    bool_val = (val != int_val);
  }

  ~test ()
  {
    if (!bool_val)
      return;
  }

  int get_int_val () const
  {
    return int_val;
  }

private:
  bool bool_val;
  int int_val;
};

static int __attribute__ ((noinline))
f1 (int i, int *p)
{
  test obj (i, p);
  return obj.get_int_val ();
}

static int __attribute__ ((noinline))
f2 (int i, int *p)
{
  test obj (i, p);
  return obj.get_int_val ();
}

int
f (int i, int *p)
{
  return f1 (i, p) + f2 (i, p);
}
