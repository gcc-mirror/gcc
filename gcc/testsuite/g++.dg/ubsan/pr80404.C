// PR sanitizer/80404
// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

extern short v;

unsigned
foo ()
{
  unsigned a = (0 < 0 >= (0 >= 0)) / (unsigned) v;
  return a;
}
