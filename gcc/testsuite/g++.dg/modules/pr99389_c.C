// { dg-additional-options -fmodules-ts }
import hello;

int main ()
{
  return Check ("World") ? 0 : 1;
}
