/* PR c/69900 */
/* { dg-do compile } */

#pragma GCC diagnostic error "-Wunreachable-code"	/* { dg-bogus "is not an option that controls warnings" } */
#pragma GCC diagnostic warning "-Wunreachable-code"	/* { dg-bogus "is not an option that controls warnings" } */
#pragma GCC diagnostic ignored "-Wunreachable-code"	/* { dg-bogus "is not an option that controls warnings" } */
