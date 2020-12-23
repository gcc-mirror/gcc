// { dg-additional-options "-fmodules-ts" }
import tom.riddle;

int main ()
{
  auto one = One (2);

  if (one (1) != 3)
    return 1;

  auto two = Two (3);
  if (two (2) != 6)
    return 2;

  return 0;
}
