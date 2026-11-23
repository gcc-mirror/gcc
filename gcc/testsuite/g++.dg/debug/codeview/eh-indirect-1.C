// { dg-do compile }
// { dg-options "-O0 -gcodeview" }

// codeview_debug_finish and write_custom_types must update in_section so
// the indirect typeinfo constant is emitted in .data after the debug info.

void f ();
extern int t;

int
g ()
{
  try { f (); }
  catch (int &) { return t; }
  return 0;
}

int t = 1;

// The initialized global must leave in_section pointing to .data.
// { dg-final { scan-assembler {t:\n[ \t]+\.long[ \t]+1\n[ \t]+\.section[ \t]+\.debug\$S} } }
// { dg-final { scan-assembler {\.data\n[ \t]+\.align[ \t]+[0-9]+\n\.LDFCM0:} } }
