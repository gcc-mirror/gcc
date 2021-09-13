// Core 1376
// PR c++/52202
// { dg-do run { target c++11 } }

extern "C" void abort();
bool x;
struct T { ~T() { if (!x) abort (); } };
int main()
{
  T&& r = static_cast<T&&>(T());
  x = true;
}
