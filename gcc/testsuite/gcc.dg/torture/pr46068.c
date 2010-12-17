/* { dg-do compile } */

void
foo ()
{
  asm goto (""::::l1);
  __builtin_unreachable ();
l1:;
}

void
bar ()
{
  foo ();
  foo ();
}
