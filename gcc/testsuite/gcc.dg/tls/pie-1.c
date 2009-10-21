/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fpie" } */
/* { dg-require-effective-target tls } */

__thread int a; int b; int main() { return a = b; }
