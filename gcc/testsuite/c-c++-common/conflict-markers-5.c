/* Ensure we can handle mismatched conflict markers.  */

int p;

=======  /* { dg-error "conflict marker" } */

int q;

=======  /* { dg-error "conflict marker" } */

int r;
