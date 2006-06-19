/* { dg-do compile  } */
/* { dg-options "-std=c99 -pedantic-errors" } */
/* Radar 4336222 */

int a;
struct s { void (*f)(int (*)[a]); };
