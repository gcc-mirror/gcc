/* { dg-options "-mabi=o64 -mlong64 -mabicalls -mno-shared -mno-plt -O2" } */
/* { dg-error "is incompatible with" "" { target *-*-* } 0 } */
#include "abi-main.h"
