// PR c++/67178
// { dg-do compile { target c++2a } }

template<typename T>
concept c = true;

template<typename T>
concept C0 = requires (auto x) { // { dg-error "placeholder type" }
  x;
};

template<typename T>
concept C1 = requires (C1 auto x) { // { dg-error "not been declared|placeholder|two or more|in requirements" }
  x; // { dg-error "not declared" }
  { x } -> c; // { dg-message "not declared|does not satisfy" }
};

template<typename T>
  requires C1<T>
void f(T) {}

int main() {
  f(1); // { dg-error "" }
}


