/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */

/* Test source Neil Booth.  GCC <= 2.96 don't get this right.  */

extern void abort (void);

int glue (int x, int y)
{
  return x + y;
}

#define glue(x, y) x ## y
#define xglue(x, y) glue (x, y)

int main ()
{
  /* Should expand to glue (1, 2) as the second "glue" is nested.  */
  if (glue (xgl, ue) (1, 2) != 3)
    abort ();

  return 0;
}
