// GROUPS passed destructors
// dtor file:
// Message-Id: <1992Jun25.181845.18886@leland.Stanford.EDU>
// From: niz@leland.stanford.edu (Jim Nisbet)
// Subject: gcc 2.2.2 -- c++ bug: destructor called twice (example)
// Date: 25 Jun 92 18:18:45 GMT

#include <stdio.h>

int things = 0;

class foo {
public:
   foo() { things++; }
   foo(const foo&) { things++; }
   ~foo() { things--; }

   int i;
};

void
sub(foo f) {
   ;
};


int main() {
   sub(foo());
   if (things == 0)
     printf ("PASS\n");
   else
     { printf ("FAIL\n"); return 1; }
}
