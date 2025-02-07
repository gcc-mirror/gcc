// PR c++/118763
// { dg-do run }

int *
foo (bool x)
{
  return new int (({ if (x) return nullptr; 1; }));
}

int
main ()
{
  delete foo (true);
  delete foo (false);
}
