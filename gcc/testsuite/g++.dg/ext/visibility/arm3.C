// { dg-do compile { target arm_eabi } }
// { dg-require-dll "" }
// { dg-options "-fvisibility=hidden" }

/* From ARM C++ ABI \S 3.2.5.5:

     A class should be exported unless explicitly tagged otherwise.

   This admonition applies even on DLL-based systems where hidden
   visibility is the default.  We want -fvisibility=hidden to behave
   identically to the situation where hidden visibility is the
   hard-wired default.  So, both A and B are exported classes.

   Furthermore:

     If CAG symbol Y names one of the impedimenta associated with an
     exported class X:

     ... 

     * Otherwise, if X has no key function:
     
       - Y is exported from ... each DLL that refers to X and uses Y.

   So, the type-info and virtual-table symbols associated with A and B
   must be exported.  */

// { dg-final { scan-not-hidden "_ZTI1A" } }
// { dg-final { scan-not-hidden "_ZTS1A" } }
// { dg-final { scan-not-hidden "_ZTV1B" } }
// { dg-final { scan-not-hidden "_ZTI1B" } }
// { dg-final { scan-not-hidden "_ZTS1B" } }

struct A {};
struct B : virtual public A {};
B b;
