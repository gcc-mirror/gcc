// Build don't link:

class error {
public:
  error(int) {}
};

class foo {
  const error x = 1; // ERROR - initialization of non-static data member
};


