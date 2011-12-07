/* { dg-do compile } */
/* { dg-options "-finline-functions" } */

int k1, k2, k3, k4, k5, k6, k7, k8;

void set_first_insn (int);
void set_last_insn (void);

static int make_insn_raw (void) 
{
  set_first_insn (0);
  set_last_insn ();
  return k1;
}

static void add_insn_after (void)
{
  if (k2)
    k3 = k4;

  if (k5)
    k6 = k7;
}

void emit_pattern_after_noloc (int (make_raw) (void)) 
{
  if (k8)
    {
      make_raw ();
      add_insn_after ();
    }
}

void emit_insn_after_noloc (void)
{
  emit_pattern_after_noloc (make_insn_raw);
}

void emit_debug_insn_before_setloc (int k9)
{
  if (k9)
    make_insn_raw ();
}
