// { dg-additional-options -fmodules-ts }
import node;

struct b
{
  int frob () 
  {
    return 0;
  }
};

int main ()
{
  assert (0);
  baselink<b> thing;
  thing.Frob ();
  if (!trait<b> ())
    return 1;
  return 0;
}
