/* PR optimization/9888 */
/* { dg-do run { target i?86-*-* } } */
/* { dg-xfail-if "" { *-*-* } { "-m64" } { "" } } */
/* { dg-options "-mtune=k6 -O3" } */

/* Verify that GCC doesn't emit out of range 'loop' instructions.  */

extern void abort (void);
extern void exit (int);

   
f1 (a)
     long a;
{
  int i;
  for (i = 0; i < 10; i++)
    {
      if (--a == -1)
	return i;
    }
  return -1;
}

f2 (a)
     long a;
{
  int i;
  for (i = 0; i < 10; i++)
    {
      if (--a != -1)
	return i;
    }
  return -1;
}

f3 (a)
     long a;
{
  int i;
  for (i = 0; i < 10; i++)
    {
      if (--a == 0)
	return i;
    }
  return -1;
}

f4 (a)
     long a;
{
  int i;
  for (i = 0; i < 10; i++)
    {
      if (--a != 0)
	return i;
    }
  return -1;
}

f5 (a)
     long a;
{
  int i;
  for (i = 0; i < 10; i++)
    {
      if (++a == 0)
	return i;
    }
  return -1;
}

f6 (a)
     long a;
{
  int i;
  for (i = 0; i < 10; i++)
    {
      if (++a != 0)
	return i;
    }
  return -1;
}


int main()
{
  if (f1 (5L) != 5)
    abort ();
  if (f2 (1L) != 0)
    abort ();
  if (f2 (0L) != 1)
    abort ();
  if (f3 (5L) != 4)
    abort ();
  if (f4 (1L) != 1)
    abort ();
  if (f4 (0L) != 0)
    abort ();
  if (f5 (-5L) != 4)
    abort ();
  if (f6 (-1L) != 1)
    abort ();
  if (f6 (0L) != 0)
    abort ();
  exit (0);
}
