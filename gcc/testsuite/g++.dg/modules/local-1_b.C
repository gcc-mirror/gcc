// { dg-additional-options "-fmodules-ts" }
import the.shop;

int main ()
{
  if (for_local_people () != 5)
    return 1;

  return 0;
}
