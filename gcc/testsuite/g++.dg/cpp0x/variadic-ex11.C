// { dg-options "-std=gnu++11" }
template<typename... Types> 
  void f(Types... args);

void g() {
  f(); // okay: args contains no arguments
  f(1); // okay: args contains one int argument
  (2, 1.0); // okay: args contains two arguments, an int and a double
}
