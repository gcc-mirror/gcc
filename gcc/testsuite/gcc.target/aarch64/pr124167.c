/* { dg-do compile } */

/* Check there is no ICE.  */

__attribute__ ((target_version ("default"))) int foo ();

int bar () { return foo (); }
