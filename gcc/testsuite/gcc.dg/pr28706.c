/* PR c/28706 */
/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -r -nostdlib" } */
/* { dg-additional-sources "pr28706.c" } */

struct A
{
  int i;
} __attribute__((aligned (sizeof (long int))));

extern void foo (struct A *);
extern void foo (struct A *);
