// { dg-do assemble  }
// PRMS Id: 3665

//-------------------------------------------------------------
//  Referential declaration within class
//
//  Imbeded below is the invocation of the compiler and the error
//  message
//
//  This compiles successfully with both the xlC and CFRONT compilers
//  This was reviewed with Clem Dickey and we agree that it appears to
//  be a Cygnus compiler problem.
//-------------------------------------------------------------
/*
$ make bug.reference.o
        /usr/p3/bin/i960-vxworks-g++ `getsrc bug.reference.C` -I. -Iinc1 -Iinc2
 -I/vw5.0.3/h  -I/vw5.0.3/h/i960 -I/usr/p3/lib/gcc-lib/i960-vxworks/cygnus-2.3.3
/include -I/usr/p3/lib/gcc-lib/i960-vxworks/cygnus-2.3.3-930417/include -I/usr/p
3/lib/i960-vxworks/include -I/usr/p3/i960-vxworks/include  -c -DCPU_FAMILY=I960
-DCPU=I960CA -mca -mold-align -g3 -O1 -DASSERT_ON -nostdinc -nostdinc++ -MD
./bug.reference.C: In method `class1::class1 (long unsigned int, long unsigned i
nt **&)':
./bug.reference.C:43: cannot convert type `long unsigned int **'
./bug.reference.C:43:        to type `long unsigned int *[]&'
make: 1254-004 The error code from the last command is 1.
*/

// typedefs
typedef unsigned long u32;
typedef u32 *ul[16];

// class defs
class class1 {
    u32   var1;
    class1(const class1 &);                   // Copy constructor
    class1& operator=(const class1 &);        // operator= member function
public:
    class1(u32, ul&);
    ul  &ulref;
    ~class1() {}
};


// member function defs
class1::class1(u32 u, ul &l) : var1(u), ulref(l)
{}

/* ===========================================================================
Note:  The following is a "work around" that allows the successful compilation.


// typedefs
typedef unsigned long u32;
typedef u32 *ul[16];

// class defs
class class1 {
    u32   var1;
    class1(const class1 &);                   // Copy constructor
    class1& operator=(const class1 &);        // operator= member function
public:
    class1(u32, ul*);
    ul  &ulref;
    ~class1() {}
};


// member function defs
class1::class1(u32 u, ul *l) : var1(u), ulref(*l)
{}
============================================================================*/
