// Build don't link: 
// GROUPS passed operators
// opr-del file
// From: Eberhard Mattes <mattes@azu.informatik.uni-stuttgart.de>
// Date:     Thu, 4 Aug 94 08:19:20 +0200
// Subject:  delete [] A::s
// Message-ID: <9408040619.AA27602@azu.informatik.uni-stuttgart.de>

class A
{
  char *s;
public:
  void f ();
};

void A::f ()
{
  delete [] A::s;
}
