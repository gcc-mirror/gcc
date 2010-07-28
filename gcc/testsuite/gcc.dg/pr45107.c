/* PR rtl-optimization/45107 */
/* { dg-do compile } */
/* { dg-options "-Os -fgcse-las" } */

extern void bar(int *);

int foo (int *p)
{
    int i = *p;
    if (i != 1)
      bar(&i);
    *p = i;
}
