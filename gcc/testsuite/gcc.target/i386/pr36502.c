/* PR target/36502 */
/* { dg-do compile { target { *-*-darwin* && ilp32 } } } */
/* { dg-options "-O -fomit-frame-pointer -fno-pic -S" } */
int a;
void f() {a++;}
/* { dg-final { scan-assembler-not "esp" } } */

