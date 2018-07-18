// PR c++/78689 - ICE on constructor with label

struct e {
  e() {
    goto aj;
    if (0)
    aj:;
  }
};

void f()
{
  struct e x;
}
