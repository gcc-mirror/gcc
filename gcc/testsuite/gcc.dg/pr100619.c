/* PR c/100619 - ICE on a VLA parameter with too many dimensions
   { dg-do compile }
   { dg-options "-Wall" } */

extern int n;

#define A10     [n][n][n][n][n][n][n][n][n][n]
#define A100    A10 A10 A10 A10 A10 A10 A10 A10 A10 A10 A10
#define A1000   A100 A100 A100 A100 A100 A100 A100 A100 A100 A100 A100

void f10 (int A10);
void f10 (int A10);

void f100 (int A100);
void f100 (int A100);

void f1000 (int A1000);
void f1000 (int A1000);

void fx_1000 (int [ ]A1000);
void fx_1000 (int [1]A1000);        // { dg-warning "-Warray-parameter" }

void fn_1000 (int [n    ]A1000);
void fn_1000 (int [n + 1]A1000);    // { dg-warning "-Wvla-parameter" }
