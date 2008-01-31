// PR c++/34935
/* { dg-do compile } */
/* { dg-final { scan-assembler "_Z1fi" } } */
/* { dg-final { scan-assembler "_Z1fb" } } */
/* { dg-final { scan-assembler "_Z1fd" } } */
/* { dg-final { scan-assembler "_Z1ff" } } */
/* { dg-final { scan-assembler "_Z1fw" } } */

typedef int INT __attribute((may_alias));

void f(int);
void f(INT) { }

typedef bool BOOL __attribute((may_alias));

void f(bool);
void f(BOOL) { }

typedef float FLOAT __attribute((may_alias));

void f(float);
void f(FLOAT) { }

typedef double DOUBLE __attribute((may_alias));

void f(double);
void f(DOUBLE) {}

typedef wchar_t WCHAR_T __attribute((may_alias));

void f(wchar_t);
void f(WCHAR_T) {}

void test()
{
  f(0);
  f(true);
  f(1.0f);
  f(1.0);
  f(L'f');
}
