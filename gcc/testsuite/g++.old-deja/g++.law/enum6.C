// { dg-do assemble  }
// GROUPS passed enums
// enum file
// From: polstra!jdp@uunet.uu.net (John Polstra)
// Date:     Mon, 15 Nov 93 12:14 PST
// Subject:  GCC 2.5.3 Enum Bitfield Causes Bogus Warning
// Message-ID: <m0ozAJo-0001EgC@seattle.polstra.uucp>

   enum Color { Red, Green, Blue };

    struct S {
        Color c : 8;
    } s;

    void foo()
    {
        s.c = Red;      // <== This statement produces the warning message.
    }

