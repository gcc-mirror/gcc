// Build don't link:

class X {
public:
  const operator int (); // ERROR - invalid declaration.
};
