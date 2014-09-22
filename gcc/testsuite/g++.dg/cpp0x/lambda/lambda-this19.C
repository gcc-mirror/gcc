// PR c++/63320
// { dg-do compile { target c++11 } }

class A {
  static void addWindow();
  static void activateWindow(void *);
};
void A::addWindow() {
  int* action {};
  [action] { activateWindow(action); };
}
