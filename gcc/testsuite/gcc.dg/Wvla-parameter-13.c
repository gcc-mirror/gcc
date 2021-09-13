/* PR c/101585 - Bad interaction of -fsanitize=undefined and -Wvla-parameters
   { dg-do compile }
   { dg-options "-Wall -fsanitize=undefined" } */

void f1 (int n, int (*)[n]);
void f1 (int n, int (*)[n]);            // { dg-bogus "\\\[-Wvla-parameter" }

void g1 (int m, int (*)[m]);
void g1 (int n, int (*)[n]);            // { dg-bogus "\\\[-Wvla-parameter" "pr101605" { xfail *-*-* } }

void h1 (int n, int (*)[n]);
void h1 (int n, int (*)[n + 1]);        // { dg-warning "\\\[-Wvla-parameter" }

void f2 (int m, int n, int (*)[m][n]);
void f2 (int n, int m, int (*)[n][m]);  // { dg-bogus "\\\[-Wvla-parameter" "pr101605" { xfail *-*-* } }

void g2 (int m, int n, int (*)[m][n]);
void g2 (int n, int m, int (*)[m][n]);  // { dg-warning "\\\[-Wvla-parameter" "pr101605" { xfail *-*-* } }
