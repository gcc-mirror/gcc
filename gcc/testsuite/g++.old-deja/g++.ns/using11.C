// Build don't link:

class joey {
public:
  typedef int SVec;
};

using joey::SVec; // ERROR - joey is not a namespace

