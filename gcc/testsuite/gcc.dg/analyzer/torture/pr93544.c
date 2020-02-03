/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

int ja;

int *
qd (void);

void
lk (void)
{
  int *bs, *dx;

  bs = dx = !!ja ? qd () : 0; /* { dg-message "following 'true' branch" } */

  __builtin_free (dx);
  __builtin_free (bs); /* { dg-warning "double-'free'" } */
}
