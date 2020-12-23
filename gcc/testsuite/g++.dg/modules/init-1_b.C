// { dg-additional-options "-fmodules-ts -fno-inline" }
import Foo;

int frob (int j)
{
  return j + 1;
}

int q = frob (j);

int main ()
{
  return !(q == 6);
}
