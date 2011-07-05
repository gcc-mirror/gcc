// PR c++/49598
// { dg-options -std=c++0x }
// { dg-do run }

int
main()
{
  int i = 10;
  int& ir = i;

  if ([=]{ return ir; }() != 10)
    return 1;
}
