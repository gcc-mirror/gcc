/* PR target/40971 */
/* { dg-do compile } */
/* { dg-options "-O -fstack-protector -fno-strict-aliasing" } */
/* { dg-require-effective-target fstack_protector } */
/* { dg-require-effective-target size20plus } */

extern void bar (char *);

void
foo (int f, long a)
{
  {
    char d[32768];
    bar (d);
  }
  double b = f;
  while (a)
    {
      char c[sizeof (double)];
      __builtin_memcpy (c, &b, sizeof (c));
      if (*(double *) c != 2.0)
	break;
    }
}
