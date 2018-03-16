// { dg-additional-options "-fmodules-ts" }
import Bar;

int main ()
{
  return frob (2, 4) != 4 * 6;
}
