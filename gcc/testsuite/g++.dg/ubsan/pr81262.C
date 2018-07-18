// PR sanitizer/81262
// { dg-do compile }
// { dg-options "-O2 -fsanitize=unreachable" }

int
foo ()
{
  asm goto ("" : : : : l1, l2);
  __builtin_unreachable ();
 l1:
  return 1;
 l2:
  return 0;
}
