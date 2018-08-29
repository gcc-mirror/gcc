// PR c++/83592
// { dg-do compile }
// { dg-options "-Wparentheses" }

// Test that -Wparentheses does not give bogus warnings in
// typename context.

int *
foo (long &a)
{
  return reinterpret_cast<int (*)> (&a);
}

int *
bar (long &a)
{
  return (int (*)) &a;
}

int *
baz (int &a)
{
  return static_cast<int (*const)> (&a);
}
