/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern" } */

struct S;
void test() { int(S::*PtrMem); }
