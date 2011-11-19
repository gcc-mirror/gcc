/* { dg-do compile } */

static float make_insn_raw (void)
{
    return 0;
}

static int emit_pattern_after_noloc (int (make_raw) ()) 
{
    return make_raw ();
}

void emit_insn_after_noloc (void) 
{
    emit_pattern_after_noloc ((void *) make_insn_raw);
}

