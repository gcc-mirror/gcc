// Build don't link: 
// GROUPS passed friends
// friends file
// From: osinski@cs.nyu.edu (Ed Osinski)
// Date:     Fri, 05 Jun 92 20:47:37 -0400
// Subject:  parameter name forgotten in certain friends
// Message-ID: <9206060047.AA05594@MURRAY.CS.NYU.EDU>

class T2;

class T {
   friend void f (int&);
};

class T2 {
      friend void f (int& i)  { // BOGUS - 
      i = 1;
   };
};
