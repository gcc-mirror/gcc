// GROUPS passed conversions
// cvt file
// Message-Id: <9308091213.AA11572@emmy.Mathematik.Uni-Dortmund.DE>
// From: Michael Strauch <strauch@emmy.mathematik.uni-dortmund.de>
// Subject: Bug in GCC 2.4.5
// Date: Mon, 9 Aug 93 14:13:50 MESZ

extern "C" int printf (const char *, ...);

int destruct = 2;

  class Test{
  protected:
     long x;
  public:
     Test(){;}
     Test(long l) {x=l;}
     ~Test() {if (--destruct == 0) printf ("PASS\n");}
  };

int main()
  {
     long i=1;
     Test t;

     t=(Test)i;
  }
