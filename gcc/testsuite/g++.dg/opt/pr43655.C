// PR tree-optimization/43655
// { dg-do run }
// { dg-options "-O0 -ftree-ter" }

extern "C" void abort ();

struct C
{
  C (int i) : val(i) { }
  C (const C& c) : val(c.val) { }
  ~C (void) { val = 999; }
  C& operator = (const C& c) { val = c.val; return *this; }
  C& inc (int i) { val += i; return *this; }
  int val;
};

C
f ()
{
  return C (3);
}

C
f (int i)
{
  return f ().inc (i);
}

int
main ()
{
  if (f (2).val != 5)
    abort ();
}
