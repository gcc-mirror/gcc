/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

int *tz;

void
ky (int);

void
wd (void)
{
  tz = 0; /* { dg-message "using NULL here" } */
  ky (*tz); /* { dg-warning "dereference of NULL" } */
}

