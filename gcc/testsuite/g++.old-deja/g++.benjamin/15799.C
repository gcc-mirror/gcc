// { dg-do assemble  }
// 981203 bkoz
// g++/15799  test1

/* 
15799.cpp: In function `void foo()':
15799.cpp:21: call of overloaded `sanjose({anonymous enum})' is ambiguous
15799.cpp:13: candidates are: sanjose::sanjose(const sanjose &) <near match>
15799.cpp:14:                 sanjose::sanjose(unsigned int)
*/

typedef char int_8;
typedef unsigned long uint_32;

class sanjose {
public:
   sanjose();
   sanjose(const sanjose&);  // { dg-message "note" }
   sanjose(int_8 value);  // { dg-message "note" }
   sanjose(uint_32 value);  // { dg-message "note" }
};

enum { first, last};

void foo(void) {
  sanjose obj(first); // { dg-error "ambiguous" }
}


