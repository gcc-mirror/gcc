// DR 2345 - Jumping across initializers in init-statements and conditions.
// { dg-do compile { target c++17 } }

int
fn ()
{
  goto X;
  if (int i = 42; i == 42)
    {
X: // { dg-error "jump to label" }
      return i;
    }
  return -1;
}
