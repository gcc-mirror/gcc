// { dg-module-do run }
// { dg-additional-options "-fmodule-header=legacy-1_a.H -fmodules-atom" }
// { dg-module-bmi /legacy-1_a.H }

#ifndef HEADER_H
#define HEADER_H

int frob (int);

#endif
