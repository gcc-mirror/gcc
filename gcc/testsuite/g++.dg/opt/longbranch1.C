// PR c++/5964
// This testcase failed to link on sparc -m64 -O0, because instruction
// lengths were incorrectly computed
// { dg-do link }
// { dg-options "-O0" }

#define makecode for (int i = 1; i < 1000; ++i) i *= 3
#define muchcode \
        makecode; makecode; makecode; makecode; makecode; makecode; \
        makecode; makecode; makecode; makecode; makecode; makecode; \
        makecode; makecode; makecode; makecode; makecode; makecode; \
        makecode; makecode; makecode; makecode; makecode; makecode

#define verymuchcode \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode; \
        muchcode; muchcode; muchcode; muchcode; muchcode; muchcode

#ifdef __mips
/* See PR 51931.  */
__attribute__((nomips16))
#endif
int
main (int argc, char **argv)
{
loop:
  verymuchcode;
  delete[] argv;
  goto loop;
}
