// PR c++/20040

class X
{
  void operator delete(void *p) throw () {} // { dg-message "declared private" }
};

X xa;

int mymain()
{
  X *p = new X; // { dg-error "is private" }
  return 0;
}
