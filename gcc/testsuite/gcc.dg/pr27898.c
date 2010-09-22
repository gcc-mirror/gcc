/* PR c/27898 */
/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -r -nostdlib" } */
/* { dg-additional-sources "pr27898.c" } */

union u { struct { int i; }; };

extern int foo (union u *);
