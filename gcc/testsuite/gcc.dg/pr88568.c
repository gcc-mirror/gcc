/* PR c/88568 */
/* { dg-do compile } */
/* { dg-require-dll "" } */
__attribute__((dllimport)) struct S var;	/* { dg-bogus "storage size of .var. isn.t known" } */
