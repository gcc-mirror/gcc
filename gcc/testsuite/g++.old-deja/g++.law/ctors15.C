// { dg-do run  }
// GROUPS passed constructors
// ctor file
// Message-Id: <9303270404.28207@munta.cs.mu.OZ.AU>
// From: fjh@cs.mu.oz.au
// Subject: bug with new/delete of multidimensional array
// Date: Sat, 27 Mar 93 14:04:52 EST

#include <stdio.h>
#include <stdlib.h>

int construct = 0;

class Element {
public:
    Element() { construct++; if (construct > 6) {printf ("FAIL\n"); exit(1);}}
    ~Element() { }
};

typedef Element array[2];

int main() {
    array *x;
    x = new array[3];
    delete x;
    printf ("PASS\n");
}
