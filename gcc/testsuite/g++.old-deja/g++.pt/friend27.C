// Build don't link:

class S
{
  friend void f<>(int); // ERROR - does not match any template
  int i;
};
