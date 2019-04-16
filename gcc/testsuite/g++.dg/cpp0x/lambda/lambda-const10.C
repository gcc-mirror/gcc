// PR c++/88613
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wtautological-compare }

void a() {
  const int b = 5;
  [=] {
    if (b != 5)
      ;
  }();
}
