// { dg-additional-options -fmodules }
import M;

int main()
{
  if (f() != 4242)
    __builtin_abort ();
}
