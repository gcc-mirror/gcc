/* Ensure we can handle mismatched conflict markers.  */

int p;

>>>>>>> Some commit message  /* { dg-error "conflict marker" } */

int q;

>>>>>>> Some other commit message  /* { dg-error "conflict marker" } */

int r;
