// { dg-do assemble  }

#define DEF_A   struct A { A() { a = 2; } int a; }

#if 1
int f1 () {
  DEF_A;
  A aa;
  return aa.a;
}

int f2 () {
  DEF_A;
  A ab;
  return ab.a;
}
/* results:
tt.cc: In function int f2 ():
tt.cc:9: conflicting types for `A::A ()'
tt.cc:3: previous declaration of `A::A ()'
/u2/projects/gcc2/src/cplus-cvt.c:1149: failed assertion `distance >= 0'
gcc2: Program cc1plus got fatal signal 6.
*/
#else

struct B1 { DEF_A; A aa; };

struct B2 { DEF_A; A aa; };
/* results:
/u2/projects/gcc2/src/cplus-decl.c:5469: failed assertion `return_type == return_ctor'
gcc2: Program cc1plus got fatal signal 6.
*/
#endif
