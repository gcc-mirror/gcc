// Build don't link: 
// Special g++ Options: -Wall -pedantic
// GROUPS passed qualifiers
class bee {
 public:
  int bee::bar;		// WARNING - there is an extra bee:: here
};

class foo {
 public:
  int bee::bar;		// ERROR - you cannot do this
    int me();
};
