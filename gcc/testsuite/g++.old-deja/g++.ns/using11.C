// { dg-do assemble  }

class joey {
public:
  typedef int SVec;
};

using joey::SVec; // { dg-error "" } joey is not a namespace

