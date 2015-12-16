// PR c++/63628
// { dg-do compile { target c++14 } }

auto const pack = [](auto&&... t)
{
  return [&](auto&& f)->decltype(auto)
  {
    return f(static_cast<decltype(t)>(t)...);
  };
};

int main(int argc, char** argv) {
  pack(1)([](int){});
  return 0;
}
