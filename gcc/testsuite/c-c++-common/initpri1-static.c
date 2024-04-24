/* { dg-do run { target init_priority } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */
/* { dg-additional-options -DCDTOR_LINKAGE=static } */

#include "initpri1.c"
