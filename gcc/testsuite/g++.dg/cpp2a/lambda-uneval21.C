// PR c++/117602
// { dg-do compile { target c++20 } }

auto x1 = [](decltype([&]{})){}; // { dg-error "non-local lambda" }

auto x2 = [&]() { // { dg-error "non-local lambda" }
     [&]() { };
};

auto x3 = []() {
     [&]() { };
};

auto x4 = []() {
     []() { };
};

auto x5 = [&]() { // { dg-error "non-local lambda" }
     []() { };
};

void
g ()
{
  [&](decltype([&](decltype([=](decltype([&](decltype([&]() {})) {})) {})) {})){};
  [&](decltype([&](decltype([&](decltype([&](decltype([&]() {})) {})) {})) {})){};
  [=](decltype([=](decltype([=](decltype([=](decltype([&]() {})) {})) {})) {})){};

  [=]() {
    [&]() { };
  };
}
