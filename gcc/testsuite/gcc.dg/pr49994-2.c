/* { dg-do compile } */
/* { dg-options "-O -fno-omit-frame-pointer -fschedule-insns2 -fsched2-use-superblocks -g" } */
/* { dg-require-effective-target scheduling } */

int
bar (int i)
{
  while (i)
    if (i)
      return i;
}

void
foo ()
{
  bar (0);
}
