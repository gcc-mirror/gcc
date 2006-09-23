/* PR c/28706 */
/* { dg-do compile } */
/* { dg-options "--combine" } */
/* { dg-additional-sources "pr28706.c" } */

struct A
{
  int i;
} __attribute__((aligned (sizeof (long int))));

extern void foo (struct A *);
extern void foo (struct A *);
