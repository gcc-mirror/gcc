/* { dg-do compile } */
/* { dg-options "-O2 -frounding-math" } */
/* { dg-add-options float16  } */
/* { dg-require-effective-target float16  } */

void f(_Complex _Float16*);
void
foo1 (_Complex _Float16 *d)
{
    _Complex _Float16 cf = 3967 + 3791 * 1i;
    f(&cf);
}
