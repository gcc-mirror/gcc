// { dg-do run  }
// { dg-options "" }
// Origin: Mark Mitchell <mark@codesourcery.com>

int main ()
{
  int i = 0;
  int j = 0;
  int k = 0;

 l:
  if (j)
    return 0;
  ({
    __label__ l; 
    k = 0;
  l: 
    if (++k <= 2)
      goto l;
    ++i;});
  if (i > 1 || k != 3)
    return 1;
  else
    {
      j = 1;
      goto l;
    }
}
