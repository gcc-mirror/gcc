// { dg-do assemble  }
// GROUPS passed operators
// opr-eq file
// Message-Id: <199311181618.AA27761@oil.cs.columbia.edu>
// From: Sam Fenster <fenster@cs.columbia.edu>
// Subject: g++ 2.5.3 can't disable assignment
// Date: Thu, 18 Nov 1993 11:18:18 -0500

class B
   {
   B &operator = (const B &);      //Disable assignment!
  public:
   virtual ~B () {}
   };

class D: public B
   {
  public:
   D () {}
   };
