// Build don't link: 
// GROUPS passed operators
// opr-ampr file
// From: Jarkko Sonninen <Jarkko.Sonninen@lut.fi>
// Date:     Thu, 7 Oct 1993 08:25:26 +0200
// Subject:  type of base class member
// Message-ID: <199310070625.AA18653@kaisa.it.lut.fi>

class A {
  public:
    int j;
};

class B: public A {
  public:
    B () {
        !(A::j);
    };
};
