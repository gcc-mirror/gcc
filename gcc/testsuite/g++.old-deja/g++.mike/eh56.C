// Special g++ Options: -fexceptions

struct A {
  A() { }
  A(const char *) { }
  A(const A&) { }
  ~A() { }
};

struct bmf {
  bmf (const A n) { }
  ~bmf (void) { }
  A name;
};

void imf (void) {
  bmf Sabs ("abs");
}

int main(void) {
  imf ();
}
