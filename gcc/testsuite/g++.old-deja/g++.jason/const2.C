// Example of static member constants

extern "C" int printf (const char *, ...);

struct T {
  static const char letter = 'a'; // this is the new stuff!
  char x[letter];
  void f();
};

void T::f() { printf ("%p", &letter); }
const char T::letter;               // still need def after class

int main() { }
