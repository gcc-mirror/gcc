// PR c++/66197
// { dg-do run { target c++14 } }
// { dg-additional-options "-fconcepts" }

extern "C" void abort();

auto add_1(auto a, auto b) { return a + b; }
auto add_2 = [](auto a, auto b) { return a + b; };

int main()
{
  if (add_1(3.5, 4) != 7.5
      || add_1(3, 4.5) != 7.5
      || add_2(3.5, 4) != 7.5
      || add_2(3, 4.5) != 7.5)
    abort();
}
