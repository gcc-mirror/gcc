// 981203 bkoz
// g++/15799  test1
// Build don't link:

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
   sanjose(const sanjose&);  
   sanjose(int_8 value);  // ERROR -  // ERROR -
   sanjose(uint_32 value);  // ERROR -  // ERROR -
};

enum { first, last};

void foo(void) {
  sanjose obj(first); // ERROR -  // ERROR -
};


