// { dg-module-do run }
// the additional fmodules-atom should not clobber the modules-legacy
// { dg-additional-options "-fmodules-legacy=legacy-1_a.H -fmodules-atom" }
// { dg-module-bmi {"legacy-1_a.H"} }

#ifndef HEADER_H
#define HEADER_H

int frob (int);

#endif
