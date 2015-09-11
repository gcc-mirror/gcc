// { dg-do assemble  }
// GROUPS passed access
// unsorted.2 file
// Date: Sat, 6 Jun 1992 18:23:03 -0400
// From: Brendan Kehoe <brendan@cs.widener.edu>
// Message-Id: <199206062223.AA22653@betty.cs.widener.edu>
// Subject: bug with access control to member functions

       class X {
	     void g (int); // { dg-message "" } is private
        public:
          void g (double);
        };

        class Y : public X { void f() { g (1); } };// { dg-error "" } 

