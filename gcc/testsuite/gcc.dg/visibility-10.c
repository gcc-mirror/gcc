/* Test that a definition marked with dllexport has default
   visibility.  */
/* { dg-require-visibility "" } */
/* { dg-require-dll "" } */
/* { dg-options "-fvisibility=hidden" } */
/* { dg-final { scan-not-hidden "g" } } */
/* { dg-final { scan-not-hidden "h" } } */
/* { dg-final { scan-not-hidden "k" } } */
/* { dg-final { scan-not-hidden "l" } } */

__declspec(dllexport) void g() {}

__declspec(dllexport) void h();
void h() {}

__declspec(dllexport) int k;

__declspec(dllexport) extern int l;
int l;
