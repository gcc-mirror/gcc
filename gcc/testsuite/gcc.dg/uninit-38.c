/* Verify that dereferencing uninitialized VLAs correctly reflects
   offsets into the objects.
   The test's main purpose is to exercise the formatting of MEM_REFs.
   If -Wuninitialized gets smarter and detects uninitialized accesses
   before they're turned into MEM_REFs the test will likely need to
   be adjusted.  Ditto if -Wuninitialized output changes for some
   other reason.
   { dg-do compile { target { { lp64 || ilp32 } || llp64 } } }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0 -fno-ipa-modref" }
   { dg-require-effective-target alloca } */

#define CONCAT(x, y)   x ## y
#define CAT(x, y)      CONCAT(x, y)
#define UNIQ(name)     CAT (name, __LINE__)

typedef __SIZE_TYPE__ size_t;

extern void* malloc (size_t);

void sink (void*, ...);

#define T(Type, idx, off)			\
  __attribute__ ((noipa))			\
  void UNIQ (test_)(int n)			\
  {						\
    char a[n], *p = a;				\
    Type *q = (Type*)((char*)p + off);		\
    sink (p, q[idx]);				\
  }						\
  typedef void dummy_type

T (int, 0, 0);      // { dg-warning "'\\*\\(int \\*\\)a' is used uninitialized" }
T (int, 0, 1);      // { dg-warning "'\\*\\(int \\*\\)\\(\\(char \\*\\)a \\+ 1\\)'" }
T (int, 0, 2);      // { dg-warning "'\\*\\(int \\*\\)\\(\\(char \\*\\)a \\+ 2\\)'" }
T (int, 0, 3);      // { dg-warning "'\\*\\(int \\*\\)\\(\\(char \\*\\)a \\+ 3\\)'" }
T (int, 0, 4);      // { dg-warning "'\\(\\(int \\*\\)a\\)\\\[1]'" }
T (int, 0, 5);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 1\\)\\)\\\[1]'" }
T (int, 0, 6);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 2\\)\\)\\\[1]'" }
T (int, 0, 7);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 3\\)\\)\\\[1]'" }
T (int, 0, 8);      // { dg-warning "'\\(\\(int \\*\\)a\\)\\\[2]'" }
T (int, 0, 9);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 1\\)\\)\\\[2]'" }


T (int, 1, 0);      // { dg-warning "'\\(\\(int \\*\\)a\\)\\\[1]' is used uninitialized" }
T (int, 1, 1);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 1\\)\\)\\\[1]'" }
T (int, 1, 2);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 2\\)\\)\\\[1]'" }
T (int, 1, 3);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 3\\)\\)\\\[1]'" }
T (int, 1, 4);      // { dg-warning "'\\(\\(int \\*\\)a\\)\\\[2]'" }
T (int, 1, 5);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 1\\)\\)\\\[2]'" }
T (int, 1, 6);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 2\\)\\)\\\[2]'" }
T (int, 1, 7);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 3\\)\\)\\\[2]'" }
T (int, 1, 8);      // { dg-warning "'\\(\\(int \\*\\)a\\)\\\[3]'" }
T (int, 1, 9);      // { dg-warning "'\\(\\(int \\*\\)\\(\\(char \\*\\)a \\+ 1\\)\\)\\\[3]'" }
