// g++ 1.37.1 bug 900519_13

// If multiple inheritance creates a situation in which a given name is
// inherited from more than one base class, and if the inherited declarations
// for the name are for different categories of members (e.g. object members,
// function members, enumeral members), then g++ will (in some cases) fail
// to flag errors when the ambiguous name is used.

// cfront 2.0 passes this test.

// keywords: inheritance, ambiguity resolution, members

struct base_0 {
  enum { base_member }; // ERROR - candidate (26, 30)
};

struct base_1 {
  int base_member;      // ERROR - candidate (26, 34)
};

struct base_2 {
  int base_member ();   // ERROR - candidate (30, 34)
};

struct derived_0 : public base_0, public base_1 {
  void member () { base_member; }			// ERROR - 
};

struct derived_1 : public base_0, public base_2 {
  void member () { base_member; }			// ERROR - missed
};

struct derived_2 : public base_1, public base_2 {
  void member () { base_member; }			// ERROR - missed
};
