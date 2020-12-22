// { dg-additional-options "-fmodules-ts" }
import One;

int y = sizeof (Bob::Y);

unsigned Foo (Bob::Y *ptr)
{
  return ptr->a + ptr->b;
}

int main ()
{
  if (y != 2 * sizeof (int))
    return 1;

  unsigned pun[4];
  pun[0] = 0xdeadbeef;
  pun[1] = 0xfeedface;
  pun[2] = 0x8badf00d;
  pun[3] = 0xcafed00d;

  copy ((Bob::Y *)pun, (Bob::Y *)&pun[2]);

  if (pun[0] != 0x8badf00d)
    return 2;
  if (pun[1] != 0xcafed00d)
    return 3;
  if (pun[2] != 0x8badf00d)
    return 4;

  if (Foo ((Bob::Y *)&pun[1]) != 0xcafed00d + 0x8badf00d)
    return 5;
  
  return 0;
}
