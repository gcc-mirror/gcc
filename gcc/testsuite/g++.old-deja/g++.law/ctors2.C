// GROUPS passed constructors
// Message-Id: <m0p8Am6-0002fCC@neal.ctd.comsat.com>
// Date: Fri, 10 Dec 93 11:33 EST
// From: neal@ctd.comsat.com (Neal Becker)
// Subject: serious problems with static constructors

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CONST const

class Sig {

public:

  enum Type { Byte_t, Word_t, Long_t, Float_t, Double_t, Complex_t, ComplexLong_t, Bad_t };

private:

  Type T;

public:

  static CONST char *ByteMagic, *WordMagic, *LongMagic,
    *FloatMagic, *DoubleMagic, *ComplexMagic, *ComplexLongMagic, *BadMagic;

  struct SigTable {
    Type T;
    CONST char* Magic;
  };

  static CONST SigTable sigTable[];

};

CONST char 
*Sig::ByteMagic = "BYTE",
*Sig::WordMagic = "WORD",
*Sig::LongMagic = "LONG",
*Sig::FloatMagic = "FLOA",
*Sig::DoubleMagic = "DOUB",
*Sig::ComplexMagic = "COMP",
*Sig::ComplexLongMagic = "CMPL",
*Sig::BadMagic = NULL;


CONST Sig::SigTable Sig::sigTable[] = {
  { Byte_t, ByteMagic },
  { Word_t, WordMagic },
  { Long_t, LongMagic },
  { Float_t, FloatMagic },
  { Double_t, DoubleMagic },
  { Complex_t, ComplexMagic },
  { ComplexLong_t, ComplexLongMagic },
  { Bad_t, BadMagic }
};

int main() {
  if (strcmp (Sig::sigTable[0].Magic, "BYTE")
      || strcmp (Sig::sigTable[1].Magic, "WORD")
      || strcmp (Sig::sigTable[2].Magic, "LONG")
      || strcmp (Sig::sigTable[3].Magic, "FLOA")
      || strcmp (Sig::sigTable[4].Magic, "DOUB")
      || strcmp (Sig::sigTable[5].Magic, "COMP"))
    printf ("FAIL\n");
  else
    printf ("PASS\n");
}
