/* Test for spurious warnings with backslashes in perverse locations.
   Bug exposed by Plumhall.  */
/* { dg-do compile } */

extern int bar;

#def\
ine foo bar

int main(void) { return foo; }
