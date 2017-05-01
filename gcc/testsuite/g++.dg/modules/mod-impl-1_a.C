// { dg-module-do "link" { xfail *-*-* } }
// FAIL until namespace hack removed, then should run

module baz [[interface]];
// { dg-module-if "baz" }

export int Square (int);

int Prod (int, int);
