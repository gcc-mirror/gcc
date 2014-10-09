/* { dg-do compile } */
enum EE
  {
    ONE, TWO, THREE
  };

int f (enum EE e)
{
  int r = 0;

#pragma GCC diagnostic push
#pragma GCC diagnostic error "-Wswitch-enum"

  switch (e)
    {
    case ONE:
      r = 1;
      break;
    case TWO:
      r = 2;
      break;
    case THREE:
      r = 3;
      break;
    }

#pragma GCC diagnostic pop

  switch (e)
    {
    case ONE:
      r = 1;
      break;
    case TWO:
      r = 2;
      break;
    }

  return r;
}
