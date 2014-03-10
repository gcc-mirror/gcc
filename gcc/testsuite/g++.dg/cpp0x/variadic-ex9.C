// { dg-do compile { target c++11 } }
template<typename... Args>              char& f(Args... args);         // #1
template<typename T1, typename... Args> short& f(T1 a1, Args... args); // #2
template<typename T1, typename T2>      int& f(T1 a2, T2 a3);          // #3

void g() {
  char& x = f();                      // calls #1
  short& y = f(1, 2, 3);              // calls #2
  int& z = f(1, 2);                   // calls #3
}
