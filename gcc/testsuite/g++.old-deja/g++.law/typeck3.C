// { dg-do assemble  }
// GROUPS passed typeck
// typeck file
// From: Dror Caspi <dror@fibronics.co.il>
// Date:     Wed, 9 Jun 1993 17:43:48 +0300
// Subject:  function typedefs in classes
// Message-ID: <199306091443.AA03735@zorba.fibronics.co.il>

class a
{
 public:
   typedef void (X)();

   X x;   // Member function of type X
};

class b
{
 public:
   typedef void (X)();   //!!!!!! g++ says : syntax error before `;'

   X x;   // Member function of type X
};
