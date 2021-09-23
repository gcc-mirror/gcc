/* PR c/101289 - bogus -Wvla-parameter warning when using const bound
   { dg-do compile }
   { dg-options "-Wall" } */

void f1ci_can (const int n, char a[n]);
void f1ci_can (const int n, char a[n]); // { dg-bogus "-Wvla-parameter" }

void f2ci_can (const int m, char a[m]);
void f2ci_can (int n,       char a[n]); // { dg-bogus "-Wvla-parameter" }

void f3i_can (int n,       char a[n]);
void f3i_can (const int n, char a[n]);  // { dg-bogus "-Wvla-parameter" }

void f4i_can (int n,       char a[n]);
void f4i_can (const int n, char a[(int)n]);   // { dg-bogus "-Wvla-parameter" }

void f5i_can (int n,       char a[(char)n]);
void f5i_can (const int n, char a[(char)n]);  // { dg-bogus "-Wvla-parameter" }

void f6i_can (int m,       char a[(char)m]);
void f6i_can (const int n, char a[(char)n]);  // { dg-bogus "-Wvla-parameter" "" { xfail *-*-* } }


/* PR c/97548 - bogus -Wvla-parameter on a bound expression involving
   a parameter */

int n;

void f7ianp1 (int, int[n + 1]);
void f7ianp1 (int, int[n + 1]);
void f7ianp1 (int, int[n + 2]);         // { dg-warning "-Wvla-parameter" }

void f8iakp1 (int k, int [k + 1]);
void f8iakp1 (int k, int [k + 1]);      // { dg-bogus "-Wvla-parameter" }
void f8iakp1 (int k, int [1 + k]);      // { dg-bogus "-Wvla-parameter" }
void f8iakp1 (int k, int [k + 2]);      // { dg-warning "-Wvla-parameter" }
