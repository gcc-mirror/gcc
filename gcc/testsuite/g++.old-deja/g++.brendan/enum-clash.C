// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// GROUPS passed arm
enum color {red, yellow, green=20, blue};
color c = 1;	// this should be an error// { dg-error "" } .*
int i = yellow;
