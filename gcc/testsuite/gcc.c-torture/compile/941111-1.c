main ()
{
  struct S { int i; char c; } obj1, obj2;

  foo ();
  if (obj1.c != obj2.c)
    bar ();
}
