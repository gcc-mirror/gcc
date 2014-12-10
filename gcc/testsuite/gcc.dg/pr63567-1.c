/* PR c/63567 */
/* { dg-do compile } */
/* { dg-options "" } */

/* Allow initializing objects with static storage duration with
   compound literals even.  This is being used in Linux kernel.  */

struct T { int i; };
struct S { struct T t; };
static struct S s = (struct S) { .t = { 42 } };
