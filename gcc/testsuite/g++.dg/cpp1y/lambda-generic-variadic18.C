// { dg-do compile { target c++14 } }

int main() {
  int x = 42;
  auto lambda2 = [x=x](int x) {}; // { dg-error "previously declared as a capture" }
  auto lambda3 = [x](auto... x) {}; // { dg-error "previously declared as a capture" }
  auto lambda4 = [](auto... x) {
    auto lambda5 = [x...](auto... x) {};  // { dg-error "previously declared as a capture" }
    auto lambda6 = [x...](int x) {};  // { dg-error "previously declared as a capture" }
  };
}
