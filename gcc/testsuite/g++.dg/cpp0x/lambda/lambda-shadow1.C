// PR c++/55357
// { dg-options "-std=c++11 -Wshadow" }

int main() {
  int x = 1;			  // { dg-message "shadowed" }
  auto const lambda = [](int x) { // { dg-warning "shadows" }
    return x;
  };
}
