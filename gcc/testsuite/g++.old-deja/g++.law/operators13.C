// { dg-do assemble  }
// GROUPS passed operators
// opr-eq file
// Message-Id: <1993Nov18.210502.28842@midway.uchicago.edu>
// From: mps@dent.uchicago.edu (Michael Spertus)
// Subject: g++ 2.5.4 bug : operator=
// Date: Thu, 18 Nov 1993 21:05:02 GMT

class T {
};

class EP {
public:
  void operator=(T *);
};


void EP::operator=(T *) { }

int main()
{
  EP ep1, ep2;
  ep1 = ep2;
}
