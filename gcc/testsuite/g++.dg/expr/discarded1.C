// PR c++/111419

struct Incomplete;

template<class T> struct Holder { T t; }; // { dg-bogus "incomplete" }

extern Holder<Incomplete> a;
extern Holder<Incomplete>& b;
extern Holder<Incomplete>* c;

int main() {
  a;
  b;
  *c;
}
