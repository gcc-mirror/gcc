// { dg-do run  }
// GROUPS passed copy-ctors
#include <stdio.h>

int pass = 0;
class name {
  int namestuff;
public:
  name() {
    namestuff = 111;
  }
  name(const name& subject);
  
  name & operator = (const name& right) {
    this->namestuff = right.namestuff;
    return *this;
  }
  
  ~name() {
    ;
  }
};

name::name(const name& subject) {
    pass = 1;
}

class person {
  int personstuff;
  name personname;
public:
  person() {
    ;
    personstuff = 222;
  }
  ~person() {
    ;
  }	
  void print() {
    ;
  }
  
};

void
test(person argp)
{
  person testp;
  
  ;
  argp.print();
  testp = argp;
  argp.print();
  testp.print();
  ;
}

int main()
{
  person mainp;
  test(mainp);
  if (pass)
    printf ("PASS\n");
  else
    { printf ("FAIL\n"); return 1; }
}

