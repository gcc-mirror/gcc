// { dg-additional-options "-fmodules-ts" }
import One;

int main ()
{
  if (Frob (2) != -2)
    return 1;

  return 0;
}
