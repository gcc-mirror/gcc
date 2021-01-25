// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: Mark Rawling <Mark.Rawling@mel.dit.csiro.au>
// Date:     Wed, 30 Jun 93 15:28:34 +1000
// Subject:  member access rule bug
// Message-ID: <9306300528.AA17185@coda.mel.dit.CSIRO.AU>
struct a {
  int aa; 
        };

class b : private a // { dg-message "" } private
{
};

class c : public b {
        int xx(void) { return (aa); }  // aa should be invisible// { dg-error "" } .*
        };

