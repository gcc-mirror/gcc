// { dg-additional-options "-fmodules-ts --param lazy-modules=1" }
import bar;

int main ()
{
  return bar::frob (0);
}
