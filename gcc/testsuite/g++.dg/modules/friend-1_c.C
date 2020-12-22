// { dg-additional-options -fmodules-ts }

import bob;

int main ()
{
  secret s (5);

  if (peeker::peek (&s) != 5)
    return 1;

  return 0;
}
