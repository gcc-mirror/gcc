/* { dg-do run { target init_priority } } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -O3" } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

#include "initpri1.c"
