// { dg-module-do "run" }

module baz [[interface]];
// { dg-module-if "baz" }

export int Square (int);

float Square (int, int);
export int Square (int, int, int);

int Prod (int, int);
