// { dg-do compile { target c++11 } }
// { dg-options "-fnon-call-exceptions" }

void foo (int *k) noexcept
{
  for (;;)
    *k = 0;
}
