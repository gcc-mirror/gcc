// PR c++/43787
// Test that we don't try to copy *x.
// { dg-do run }

class empty_t {};

int main()
{
  empty_t* x = 0;
  empty_t y = *x;
}
