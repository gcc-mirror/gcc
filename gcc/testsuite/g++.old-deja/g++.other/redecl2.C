// Build don't link:

struct S {
  S(int);
  S(int); // ERROR - already declared

  ~S();
  ~S(); // ERROR - already declared
};
