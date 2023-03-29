void fun1() {
  int x = 0;
  [[ assert: x < 0 ]];
}
namespace tns {
  void fun2() {
    fun1();
  }
}
template<typename T>
void fun3(T a) {
  tns::fun2();
}
void fun4() {
  fun3(5);
}
int main(int, char**) {
  void (*fp)() = nullptr;
  fp = fun4;
  fp();
  return 0;
}

