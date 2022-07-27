// PR c++/105885
// { dg-do compile { target c++17 } }
// { dg-additional-options -Wall }

int i;

template<const char* ARG = nullptr>
void test() {
  if constexpr(ARG == nullptr) {
    ++i;
  } else {
    --i;
  }
}

const char CONSTSTR[] = {'\n', '\t', ' ', '\0'};

int main() {
  test();
  test<CONSTSTR>();
}
