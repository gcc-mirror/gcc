/* This really should use "dg-do compile" without the -S in dg-options,
   but the extra options get put after the input file in that case, and
   hence the test would fail. */
/* { dg-do assemble } */
/* { dg-options "-S -x c++-header" } */

struct s {
	unsigned field;
};
