// PR c++/49598
// { dg-do run { target c++11 } }

int
main()
{
  int i = 10;
  int& ir = i;

  if ([=]{ return ir; }() != 10)
    return 1;
}
