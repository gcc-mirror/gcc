// Build don't link:

struct S
{
  friend void f<>(int); // ERROR - does not match any template
};
