extern int i __attribute__((unused)) = 0;  // { dg-warning "12:.i. initialized and declared .extern." }

void foo()
{
  extern int i __attribute__((unused)) = 0;  // { dg-error "14:.i. has both .extern. and initializer" }
}
