/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */

void foo1(int i, int j) __attribute__((fastcall, cdecl)); /* { dg-error "not compatible" } */
void foo2(int i, int j) __attribute__((fastcall, stdcall)); /* { dg-error "not compatible" } */
void foo3(int i, int j) __attribute__((fastcall, regparm(2))); /* { dg-error "not compatible" } */
void foo4(int i, int j) __attribute__((stdcall, cdecl)); /* { dg-error "not compatible" } */
void foo5(int i, int j) __attribute__((stdcall, fastcall)); /* { dg-error "not compatible" } */
void foo6(int i, int j) __attribute__((cdecl, fastcall)); /* { dg-error "not compatible" } */
void foo7(int i, int j) __attribute__((cdecl, stdcall)); /* { dg-error "not compatible" } */
void foo8(int i, int j) __attribute__((regparm(2), fastcall)); /* { dg-error "not compatible" } */

