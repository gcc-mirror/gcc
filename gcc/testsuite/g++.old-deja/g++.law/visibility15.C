// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: wpsun4!xinforms!johnjo@uunet.uu.net (John D. Johnson)
// Date:     Wed, 4 Aug 93 13:25:25 MDT
// Subject:  Access to private 'operator new()'
// Message-ID: <9308041925.AA09825@xinforms.wpunix
#include <stdio.h>
#include <sys/types.h>

class X {
private:
  void* operator new(size_t) throw(){// { dg-message "" } .*
    printf("Inside private new().\n");
    return NULL;
  }
public:
  X() {}
};


int main(void)
{
  X* p = new X;// { dg-error "" } .*
}
