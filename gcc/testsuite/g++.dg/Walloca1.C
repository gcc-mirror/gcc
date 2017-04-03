/* PR middle-end/79809 */
/* { dg-do compile } */
/* { dg-options "-Walloca-larger-than=4207115063 -Wvla-larger-than=1233877270 -O2" } */

int a;
char *b = static_cast<char *>(__builtin_alloca (a)); // { dg-warning "argument to .alloca. may be too large|unbounded use of" }
