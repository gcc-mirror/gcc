// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>

  const char *pc;
  enum A { x } a;
  int i;

  int main()
  {
     return i ? *pc : a;
  }
