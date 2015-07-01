// { dg-do compile }
// { dg-options "-std=c++0x -fnon-call-exceptions" }

void foo (int *k) noexcept
{
  for (;;)
    *k = 0;
}
