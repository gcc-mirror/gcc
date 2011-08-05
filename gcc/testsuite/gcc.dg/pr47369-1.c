/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fPIC" } */

struct FILE
{
  int x;
};
extern struct FILE __sF[];
extern void bar (struct FILE *);
void dlmalloc_stats() {
   bar ((&__sF[2]));
}
