/* PR119692 "C++ 'typeinfo', 'vtable' vs. OpenACC, OpenMP 'target' offloading" */

/* { dg-do run } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

#include "../../../../libgomp/testsuite/libgomp.oacc-c++/pr119692-1-1.C"
