/* Taken from gcc.dg/pr70022.c, adding -O1 to the options
   (and -fanalyzer, implicitly).  */

/* { dg-do compile } */
/* { dg-options "-w -Wno-psabi -O1" } */

typedef int v4si __attribute__ ((vector_size (16)));

int
foo (v4si v)
{
  return v[~0UL];
}
