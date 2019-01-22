// { dg-additional-options -fmodules-ts }
import node;

struct b
{
  int frob () const
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

  if (!ptrmemdata (other ()))
    return 2;

  if (ptrmemfn (b ()))
    return 2;

  return 0;
}
