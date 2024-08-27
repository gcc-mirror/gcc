// PR c++/116424
// { dg-do compile { target c++20 } }

struct dd {
  char *ptr;
  dd();
  dd(dd &&__str);
};
struct v {
  dd n{};
  int f = -1;
  v operator|(const v &other) const;
};
struct cc : v {};
static const cc a;
static const cc b;
static const cc c1(a | b);
static const cc c2{a | b};
static const cc c3 = cc(a | b);
static const cc c4 = cc{a | b};
