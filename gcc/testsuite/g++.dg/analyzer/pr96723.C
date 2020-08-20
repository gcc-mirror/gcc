void
foo ()
{
  union
  {
    int *p;
  } u;
  u.p = new int;
  delete u.p;
}
