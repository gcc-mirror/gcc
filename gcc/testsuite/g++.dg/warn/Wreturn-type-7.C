// PR c++/20681
// { dg-options -Wreturn-type }

struct a{~a();a();};
int GetMetaCombination (int a2)
{
  a bi;
  switch (a2)
  {
    case 1:
      return 18;
      break;//removing this works around the warning
    default:
      return 0;
  }
}
