/* PR target/94121 */
/* { dg-do compile { target pie } } */
/* { dg-options "-O2 -fpie -w" } */

#define DIFF_MAX __PTRDIFF_MAX__
#define DIFF_MIN (-DIFF_MAX - 1)

extern void foo (char *);
extern char v[];

void
bar (void)
{
  char *p = v;
  foo (&p[DIFF_MIN]);
}
