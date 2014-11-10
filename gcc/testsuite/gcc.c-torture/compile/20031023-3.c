/* Declaration of the frame size doesn't work on ptx.  */
/* { dg-require-effective-target untyped_assembly } */
#define ASIZE 0x100000000UL
#include "20031023-1.c"
