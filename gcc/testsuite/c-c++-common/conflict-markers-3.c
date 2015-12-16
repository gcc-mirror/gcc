/* Ensure we can handle unterminated conflict markers.  */

int p;

<<<<<<< HEAD  /* { dg-error "conflict marker" } */

int q;

<<<<<<< HEAD  /* { dg-error "conflict marker" } */

int r;
