// { dg-do assemble  }
// GROUPS passed constructors
// ctor file
// From: rac@qedinc.com (Robert Clark)
// Date:     Tue, 14 Dec 93 10:45:50 PST
// Subject:  bug in g++ 2.5.7 Array of objects
// Message-ID: <9312141845.AA09188@annapurna.qedinc.com>


class POOL {
public:
  POOL();
};

struct VAL_LIST {
  POOL   pool[2];
};

VAL_LIST baz;

