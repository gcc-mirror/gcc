// { dg-do assemble  }
// { dg-options "-fshort-enums" }
// GROUPS passed enums
// enum file
// From: Jeff Gehlhaar <jbg@qualcomm.com>
// Date:     Mon, 4 Jan 1993 09:20:50 -0700
// Subject:  Enum bug, affects library
// Message-ID: <9301041720.AA17340@harvey>

typedef unsigned long __fmtflags;
class ios {
  public:
    typedef __fmtflags fmtflags;
    enum { skipws=01, left=02, right=04, internal=010,
           dec=020, oct=040, hex=0100,
           showbase=0200, showpoint=0400, uppercase=01000, showpos=02000,
           scientific=04000, fixed=010000, unitbuf=020000, stdio=040000,
           dont_close=0x80000000 //Don't delete streambuf on stream destruction
           };

    fmtflags setf (fmtflags, fmtflags);
};

// from <iostream.h>
inline ios& dec(ios& i)
{ i.setf(ios::dec, ios::dec|ios::hex|ios::oct); return i; }

