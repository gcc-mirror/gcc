// { dg-do run  }
// GROUPS passed operators
// opr-new file
// From: (The Crossjammer) <xjam@cork.cs.berkeley.edu>
// Date:     Mon, 23 Nov 92 23:35:26 PST
// Subject:  g++-2.3.1 : Incorrectly calls overloaded operator new
// Message-ID: <9211240735.AA06872@cork.CS.Berkeley.EDU>


#include <stdio.h>
#include <stdlib.h>

class blah {
     int j;
   public:
     blah();
     void *operator new(size_t size) throw();
};

inline blah::blah() : j(0) {
	  
}


void *blah::operator new(size_t size) throw(){
     printf ("FAIL\n");
     exit (1);
     return NULL;
}

int main(int arg, char** argv) {
     blah* blahPtr;

     blahPtr = new blah[100];
     printf ("PASS\n");
}
