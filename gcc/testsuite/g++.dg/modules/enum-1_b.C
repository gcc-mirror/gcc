// { dg-additional-options "-fmodules-ts" }
import enUm;

Bill x = Three;
Ben y = Ben::Three;

int main ()
{
  if (x != 3)
    return 1;

  if (int (y) != 3)
    return 2;

  if (int (func1 ()) != 3)
    return 3;

  if (int (func2 ()) != 4)
    return 4;

  return 0;
}
