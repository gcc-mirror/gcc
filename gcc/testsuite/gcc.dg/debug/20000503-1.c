/* { dg-do compile } */

/* Distilled from GCC's stmt.c.  Caused abort in dwarf-1 code. */

static void
expand_nl_goto_receiver ()
{
{
static struct elims {int from, to;} elim_regs[] = {{ 16, 7}, { 16, 6}, { 20, 7},{ 20, 6}};
      int i;

      for (i = 0; i < sizeof elim_regs / sizeof elim_regs[0]; i++)
	if (elim_regs[i].from == 16 && elim_regs[i].to == 6)
	break;
      }
}
