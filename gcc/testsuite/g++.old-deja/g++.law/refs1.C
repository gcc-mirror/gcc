// { dg-do run  }
// GROUPS passed references
// (Message bugs/refs:1)
// From: tal@vlsi.cs.caltech.edu
// Date:     Fri, 25 Feb 94 23:55:50 -0800
// Subject:  g++-2.5.8 produces incorrect code for references
// Message-ID: <9402260755.AA27693@vlsi.cs.caltech.edu>

#include <stdio.h>

class C {
private:
   char** list;
public:
   C(char** );
   void count (int&);
};

C::C (char** l) {
   list = l;
}

void C::count (int& total) {
   if (*list == NULL)
      return;
   else {
      list++;
      count (++total); // THIS IS WHERE THE TROUBLE STARTS
   }
}

char * foo[] = {
   "one", "two", "three", NULL};

int main() {
   C c(foo);
   int i = 0;
   c.count(i);
   if (i == 3)
     printf ("PASS\n");
   else
     { printf ("FAIL\n"); return 1; }
}
