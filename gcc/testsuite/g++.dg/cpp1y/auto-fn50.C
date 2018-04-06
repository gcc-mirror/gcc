// PR c++/84906
// { dg-do compile { target c++14 } }

extern "C" int puts(const char*);

struct aa {
  operator auto() {
    puts("auto");
    return false;
  }
  explicit operator bool() {
    puts("bool");
    return true;
  }
};

int main() {
  aa x;
  if (x)			// { dg-error "ambiguous" }
    puts("here");
  else
    puts("there");
}
