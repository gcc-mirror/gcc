// { dg-module-do "run"  { xfail *-*-* } }
// FIXME: set decl module on import

module baz [[interface]];
// { dg-module-if "baz" }

export int Square (int);

int Prod (int, int);
