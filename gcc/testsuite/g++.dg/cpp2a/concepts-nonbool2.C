// { dg-do compile { target concepts } }

template<class X, X x>
concept C = requires {
    requires x;			// { dg-error "bool" }
  };

int main() {
  C<int, 0>;
  return 0;
}
