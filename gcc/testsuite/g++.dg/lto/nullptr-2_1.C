extern int x; // { dg-lto-warning "type of 'x' does not match original declaration" }

int
get (void)
{
  return x;
}
