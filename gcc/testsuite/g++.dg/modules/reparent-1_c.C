// { dg-additional-options -fmodules-ts }
import bar;

int main ()
{
  v (0); // { dg-error "too many arguments" }
}

// { dg -regexp {In module foo, imported at [^\n]*/reparent-1_b.C:14,\nof module bar, imported at [^\n]*/reparent-1_c.C:2:\n[^\n]*/reparent-1_a.C:6:13: note: declared here\n} }
