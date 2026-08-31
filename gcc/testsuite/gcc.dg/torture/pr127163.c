/* { dg-do compile } */

/* Check there is no ICE.  */
_Bool b;
_Complex unsigned c;
void foo() { c /= (_Bool) + b; }

