/* { dg-options "-mabi=64 -mlong32 -mabicalls -mno-shared -mno-plt" } */
/* { dg-error "is incompatible with" "" { target *-*-* } 0 } */
#include "abi-main.h"
