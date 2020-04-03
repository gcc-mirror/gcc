// { dg-additional-options -fmodules-ts }
import nsdmi;

int main ()
{
  Bob b;

  return b.m != 42;
}
