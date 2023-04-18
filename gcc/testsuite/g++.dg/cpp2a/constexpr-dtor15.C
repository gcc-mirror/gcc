// PR c++/109357
// { dg-do compile { target c++20 } }
// { dg-prune-output "used but never defined" }

struct basic_string {
  char _M_local_buf;
  basic_string();
  constexpr basic_string(const char *) {}
  constexpr ~basic_string();
  constexpr basic_string& operator=(basic_string);
};
struct S1 {
  basic_string x;
  basic_string y;
} s1;
struct s2 {
  ~s2();
};
s2::~s2() { s1 = {"", ""}; }
