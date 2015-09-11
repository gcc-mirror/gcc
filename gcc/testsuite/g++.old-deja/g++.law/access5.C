// { dg-do assemble  }
// GROUPS passed access
// access file
// From: Jeffrey C. Gealow <jgealow@mtl.mit.edu>
// Date:     Thu, 18 Feb 93 10:22:23 -0500
// Subject:  nested class access control bug
// Message-ID: <9302181522.AA29209@mtl.mit.edu>


class enclose {
public:
  class nested_public { int x; };
protected:
  class nested_protected { int x; };
private:
  class nested_private { int x; }; // { dg-message "" } private
};

class derived : public enclose {
  nested_public obj1;     // ok
  nested_protected obj2;  // ok
  nested_private obj3;    // error// { dg-error "" } in this context
};

