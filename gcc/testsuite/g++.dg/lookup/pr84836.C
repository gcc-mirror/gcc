// PR c++/84836
// ICE popping local binding

void foo (void)
{
  struct A;
  void A (int);
  void A (long);
}
