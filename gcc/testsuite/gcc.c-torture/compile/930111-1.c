/* { dg-additional-options "-std=gnu89" } */

/* 2.3.3 crashes on 386 with -traditional */
f(a)
     char *a;
{
  int d = strcmp(a,"-");

  while (vfork() < 0)
    ;
  return d;
}
