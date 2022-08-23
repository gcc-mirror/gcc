// PR c++/54883
// { dg-additional-options "-fno-pie" { target ia32 } }

namespace { enum E { E1 }; } void f(E e) { }

// { dg-final { scan-assembler-not "globl" { xfail { powerpc-ibm-aix* } } } } 
