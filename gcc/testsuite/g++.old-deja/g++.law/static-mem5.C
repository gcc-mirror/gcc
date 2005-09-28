// { dg-do assemble  }
// { dg-options "-w -fpermissive" }
// GROUPS passed static-mem
// static-mem file
// From: bunch@tazboy.jpl.nasa.gov (Walt Bunch)
// Date:     Thu, 23 Jun 94 14:58:35 UNI
// Subject:  bug report
// Message-ID: <9406232258.AA03897@tazboy.JPL.NASA.GOV>


class A
{
public:
  static void F (int i) {}
  static void A::G (int i) {}
};

main ()
{
  A::F (17);
  A::G (42);
}

