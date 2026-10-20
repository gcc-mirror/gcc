/* This really should use "dg-do compile" without the -S in dg-options,
   but the extra options get put after the input file in that case, and
   hence the test would fail.  Search for "This is obscure" in
   /usr/share/dejagnu/target.exp.  Maybe we should add a dg-early-options
   to pass early_flags= to target_compile?  */
/* { dg-do assemble } */
/* { dg-options "-S -x c++-header" } */

struct s {
	unsigned field;
};
