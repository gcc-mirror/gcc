// PR c++/19628
// Verify that __builtin_constant_p may appear in a constant-expression.

// { dg-do run }

int main()
{
  switch (3) {
  case (__builtin_constant_p(7) ? 3 : 8):
    return 0;
  default:
    return 1;
  }
}
