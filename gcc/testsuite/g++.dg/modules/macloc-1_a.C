
// { dg-module-bmi agnes }

export module agnes;

int a;

#define BOB(X) int X ()
#define KEVIN(X) int X ()

export BOB(me);
export KEVIN(you);

