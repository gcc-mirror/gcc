/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

#define FN(C)		\
  void			\
  fn (void)		\
  {			\
    C;			\
  }

int i;

FN (if (i) ++i)
