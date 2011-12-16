/* { dg-do compile } */
/* { dg-options "-O -fexceptions -fnon-call-exceptions" } */

extern void f2 () __attribute__ ((noreturn));
void
f1 ()
{
  unsigned char a[8];
  unsigned int i;

  for (i = 0; i < 8; i++)
    {
      if (i > 8)
	f2 ();
      a[i] = i <= 8;
    }
}
