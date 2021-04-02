// { dg-module-do "run" }
// { dg-additional-options "-fmodules-ts" }

export module baz;
// { dg-module-cmi "baz" }

export int Square (int);

float Square (int, int);
export int Square (int, int, int);

int Prod (int, int);
