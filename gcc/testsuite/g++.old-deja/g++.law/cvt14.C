// { dg-do assemble  }
// GROUPS passed conversions
// cvt file
// Message-Id: <9303241504.AA25988@spock.orl.mmc.com>
// Subject: internal compiler error
// Date: Wed, 24 Mar 1993 10:04:06 -0500
// From: "Malcolm C. Strickland" <chucks@orl.mmc.com>


int main()
        {
          double *d;
          d = new double(10);
          return 1;
        }
