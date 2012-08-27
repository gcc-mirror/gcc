/* { dg-options "-mabi=32 -mlong64 -mabicalls -mno-shared -mno-plt" } */
/* { dg-error "is incompatible with" "" { target *-*-* } 0 } */
#include "abi-main.h"
