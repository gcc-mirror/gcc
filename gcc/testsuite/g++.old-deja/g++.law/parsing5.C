// { dg-do assemble  }
// GROUPS passed parsing
// From: Teemu Torma <Teemu.Torma@frend.fi>
// Date:     Mon, 15 Jun 92 18:42:25 +0300
// Subject:  G++ 2.2.2: Strange parse error
// Message-ID: <"relay.fren.501:15.05.92.15.42.30"@frend.fi>

long (*foo1 ()) (); // This is ok.

typedef long INT;
INT (*foo2 ()) ();  // This is not, g++ says "parse error before `('"
