// { dg-module-do run }
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#ifndef MY_IOSTREAM_H
#define MY_IOSTREAM_H
/* Proxy iostream, because command line in flux.  */
#include <iostream>

#endif
