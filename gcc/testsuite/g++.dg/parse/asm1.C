//PR c++/30849

void foo()
{
  asm("" : 0);  // { dg-error "numeric constant|token" }
}
