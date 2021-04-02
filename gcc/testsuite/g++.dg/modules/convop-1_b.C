// { dg-additional-options "-fmodules-ts" }
import frob;

int main ()
{
  A a;

  if (static_cast<int> (a))
    return 1;
  return 0;
}
