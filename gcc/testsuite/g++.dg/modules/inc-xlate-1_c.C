// { dg-additional-options -fmodules-ts }

extern "C" {
  #include "inc-xlate-1_a.H"
}

int main ()
{
  frob ();
  return 0;
}
