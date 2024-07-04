// PR c++/111160
// { dg-do compile { target c++11 } }

struct TheClass {
  TheClass() {}
  TheClass(volatile TheClass& t) {}
  TheClass operator=(volatile TheClass& t) volatile { return t; }
};
void the_func() {
  volatile TheClass x, y, z;
  (false ? x : y) = z;
}
