// { dg-additional-options "-fmodules-ts" }

export module agnes;
// { dg-module-cmi agnes }

int a;

#define BOB(X) int X ()
#define KEVIN(X) int X ()

export BOB(me);
export KEVIN(you);

