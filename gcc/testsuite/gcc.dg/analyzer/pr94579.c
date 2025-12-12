struct a *c;
struct a {
  int b;
} d() {
} /* { dg-warning "use of uninitialized value '<return-value>'" } */

void e()

{
  *c = d();
}
