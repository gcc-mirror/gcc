/* { dg-do compile { target { ! ia32 } } } */

void foo1(int i, int j) __attribute__((regparm(0))); /* { dg-warning "ignored" } */
void foo2(int i, int j) __attribute__((stdcall)); /* { dg-warning "ignored" } */
void foo3(int i, int j) __attribute__((fastcall)); /* { dg-warning "ignored" } */
void foo4(int i, int j) __attribute__((cdecl)); /* { dg-warning "ignored" } */
void foo5(int i, int j) __attribute__((thiscall)); /* { dg-warning "ignored" } */
void foo6(int i, int j) __attribute__((sseregparm)); /* { dg-warning "ignored" } */
