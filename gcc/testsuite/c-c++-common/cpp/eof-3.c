/* PR preprocess/95183  */

/* { dg-do preprocess } */
/* { dg-additional-options "-include $srcdir/c-c++-common/cpp/eof-2.h" } */

 /* { dg-regexp {[^\n]*eof-2.h:4:21: error: unterminated argument list invoking macro "f"\n} } */

token )
