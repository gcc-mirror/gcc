// { dg-do run  }
// GROUPS passed code-generation
// code-gen file
// From: david.binderman@pmsr.philips.co.uk
// Date:     Tue, 17 Aug 93 10:09:38 BST
// Subject:  .* broken in 2.4.5
// Message-ID: <9308170909.AA05509@pmsr.philips.co.uk>

class A {
public:
        char c;
};

typedef char A::*PMA;

PMA pmA = &A::c;

A oA;

extern "C" int printf( const char *, ...);

int main()
{
        oA.c = 'q';

        if ( (oA .* pmA))
                        printf( "PASS\n");
        else
                        printf(" FAIL\n");
}

