/* PR c/34668 */
/* { dg-do compile } */
/* { dg-options "--combine -O2" } */
/* { dg-additional-sources "pr34668-2.c" } */

struct optab { unsigned code; };
extern struct optab optab_table[1];

void
init_optab (struct optab *op)
{
  op->code = 0xdead;
}

void
set_conv_libfunc (void)
{
  init_optab (&optab_table[0]);
}
