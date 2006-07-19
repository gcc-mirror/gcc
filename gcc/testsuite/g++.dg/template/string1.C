// PR c++/28337

template <int> void foo()
{
  (0 ? "" : "X") + 1;
}

