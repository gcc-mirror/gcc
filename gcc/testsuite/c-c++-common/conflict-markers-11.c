/* Verify that we only report conflict markers at the start of lines.  */
int p;

 <<<<<<< HEAD /* { dg-error "expected identifier|expected unqualified-id" } */

int q;

 =======      /* { dg-error "expected identifier|expected unqualified-id" } */

int r;

 >>>>>>> Some commit message  /* { dg-error "expected identifier|expected unqualified-id" } */

int s;
