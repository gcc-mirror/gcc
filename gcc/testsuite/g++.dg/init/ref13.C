// PR c++/28338

void foo()
{
  { static const int& i = 0; }
  { static const int& i = 0; }
}
