// PR c++/55357
// { dg-do compile { target c++11 } }
// { dg-options "-Wshadow" }

int main() {
  int x = 1;			  // { dg-message "shadowed" }
  auto const lambda = [](int x) { // { dg-warning "shadows" }
    return x;
  };
}
