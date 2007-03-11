// PR c++/30274
// { dg-do run }

struct S {
  bool x : 4;
};

S s;

int main() {
  s.x++;
  if (s.x != 1)
    return 1;
  ++s.x;
  if (s.x != 1)
    return 2;
}
