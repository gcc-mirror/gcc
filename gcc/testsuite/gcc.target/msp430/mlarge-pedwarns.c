/* { dg-do compile } */
/* { dg-skip-if "" { "*-*-*" } { "-mcpu=msp430" } { "" } } */
/* { dg-options "-mlarge -pedantic-errors" } */

/* Ensure the use of builtin macros that contain __int20__ in their
   expansion don't cause ISO C errors when -pedantic-errors is passed.  */

__SIZE_TYPE__ a;
__INTPTR_TYPE__ b;
__UINTPTR_TYPE__ c;
__PTRDIFF_TYPE__ d;
