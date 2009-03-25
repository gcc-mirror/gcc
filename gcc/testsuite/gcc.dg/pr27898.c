/* PR c/27898 */
/* { dg-do compile } */
/* { dg-options "--combine" } */
/* { dg-additional-sources "pr27898.c" } */

union u { struct { int i; }; };

extern int foo (union u *);
