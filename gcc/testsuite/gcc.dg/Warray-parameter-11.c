/* PR c/101702 - ICE on invalid function redeclaration
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __INTPTR_TYPE__ intptr_t;

#define copysign(x, y) __builtin_copysign (x, y)

void f0 (double[!copysign (~2, 3)]);

void f1 (double[!copysign (~2, 3)]);
void f1 (double[1]);                    // { dg-warning "-Warray-parameter" }

void f2 (int[(int)+1.0]);
void f2 (int[(int)+1.1]);

/* Also verify that equivalent expressions don't needlessly cause false
   positives or negatives.  */
struct S { int a[1]; };
extern struct S *sp;

void f3 (int[(intptr_t)((char*)sp->a - (char*)sp)]);
void f3 (int[(intptr_t)((char*)&sp->a[0] - (char*)sp)]);
void f3 (int[(intptr_t)((char*)&sp->a[1] - (char*)sp)]);   // { dg-warning "-Warray-parameter" }
