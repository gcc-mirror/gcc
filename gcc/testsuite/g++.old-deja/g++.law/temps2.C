// GROUPS passed temps
// temps file
// Message-Id: <9212181914.AA05066@sparc1.cnm.us.es>
// From: juando@cnm.us.es (Juan Domingo Martin Gomez)
// Subject: Temporaries destroyed too soon
// Date: Fri, 18 Dec 92 20:14:45 +0100

#include <stdio.h>

int status = 0;
int fail = 0;

class Foo
{
public:
    Foo();
    ~Foo();

    Foo &method();
};

Foo f1()
{
    return Foo();
}

Foo::Foo()
{
}

Foo::~Foo()
{
    if (status == 2)
      fail = 0;
    else
      fail = 1;
}

Foo &Foo::method()
{
    status++;
    return *this;
}

int main()
{
    // f1() returns a temporary object. The member function
    // method() returns a reference to the same object.
    f1().method().method();
    if (fail)
      printf ("FAIL\n");
    else
      printf ("PASS\n");
}
