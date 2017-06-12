// PR sanitizer/80405
// { dg-do compile }
// { dg-options "-fsanitize=undefined" }

extern unsigned int v, w;

void
foo ()
{
  w = (!~0 >= (unsigned) (0 < 0)) << v;
}
