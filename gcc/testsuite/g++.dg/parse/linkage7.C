// { dg-do compile }
// The main function shall not be declared with a linkage-specification
// other than "C++".

extern "C++" {
  int main();
}
