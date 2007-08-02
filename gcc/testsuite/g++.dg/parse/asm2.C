//PR c++/30850

void foo()
{
  asm("" :: 0);  // { dg-error "numeric constant|token" }
}
