// { dg-module-do run }
// { dg-additional-options "-fmodule-legacy=legacy-1_a.H -fmodules-ts" }
// { dg-module-bmi {"legacy-1_a.H"} }

#ifndef HEADER_H
#define HEADER_H

int frob (int);

#endif
