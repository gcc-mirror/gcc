// { dg-additional-options -fmodules-ts }

import foo;

struct Mine : Visitor 
{
  int Visit () override
  {
    return 1;
  }
};

int main ()
{
  Mine me;

  return !(Visit (&me) == 1);
}
