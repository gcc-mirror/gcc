/* Test for case labels not integer constant expressions but folding
   to integer constants (used in Linux kernel).  */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

extern unsigned int u;

void
b (int c)
{
  switch (c)
    {
    case (int) (2  | ((4 < 8) ? 8 : u)): /* { dg-warning "case label is not an integer constant expression" } */
      ;
    }
}

