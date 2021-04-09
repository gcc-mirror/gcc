/* PR c/99588 */
/* { dg-do compile } */
/* { dg-options "-std=c11 -Wunused-but-set-variable" } */

void bar (int, ...);
void f1 (void) { static _Atomic int x = 0; bar (0, x); }
void f2 (void) { static _Atomic int x = 0; bar (0, x += 1); }
void f3 (void) { static _Atomic int x = 0; bar (x); }
void f4 (void) { static _Atomic int x = 0; bar (x += 1); }
void f5 (void) { static _Atomic int x = 0; bar (x = 1); }
void f6 (void) { static _Atomic int x = 0; x = 1; }	/* { dg-warning "variable 'x' set but not used" } */
void f7 (void) { static _Atomic int x = 0; x += 3; }
void f8 (void) { _Atomic int x = 0; bar (0, x); }
void f9 (void) { _Atomic int x = 0; bar (0, x += 1); }
void f10 (void) { _Atomic int x = 0; bar (x); }
void f11 (void) { _Atomic int x = 0; bar (x += 1); }
void f12 (void) { _Atomic int x = 0; bar (x = 1); }
void f13 (void) { _Atomic int x = 0; x = 1; }		/* { dg-warning "variable 'x' set but not used" } */
void f14 (void) { _Atomic int x = 0; x += 3; }
void f15 (void) { _Atomic int x = 0; int y = 3; x += y; }
void f16 (void) { _Atomic int x = 0; int y = 3; bar (x += y); }
void f17 (void) { _Atomic int x = 0; int y = 3; x = y; }	/* { dg-warning "variable 'x' set but not used" } */
void f18 (void) { _Atomic int x = 0; int y = 3; bar (x = y); }
