/* { dg-do compile } */
/* { dg-options "-std=c23" { target c } } */
/* { dg-options "-std=gnu++11" { target c++ } } */

[[musttail]] int j; /* { dg-warning "attribute" } */
__attribute__((musttail)) int k; /* { dg-warning "attribute" } */

void foo(void)
{
  [[gnu::musttail]] j++; /* { dg-warning "attribute" } */
  [[gnu::musttail]] if (k > 0) /* { dg-warning "attribute" } */
    [[gnu::musttail]] k++; /* { dg-warning "attribute" } */
}

int foo2(int p)
{
  [[gnu::musttail(1)]] return foo2(p + 1); /* { dg-error "\(before numeric constant|attribute\)" } */
}

int i;

int foo3(void)
{
  [[musttail]] i++; /* { dg-warning "attribute" } */
  [[musttail]] if (i > 10) /* { dg-warning "attribute" } */
    [[musttail]] return foo2(i); /* { dg-warning "attribute" } */
  return 0;
}
