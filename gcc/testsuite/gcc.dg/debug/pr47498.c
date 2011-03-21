/* { dg-do compile } */
/* { dg-options "-O2 -fsched2-use-superblocks -fcompare-debug" } */

int bar(void *);

void foo (void *p)
{
  int i = 1;
  while (i)
    i = bar (p);
}
