/* Test that -gprune-btf does not prune at typedefs.  */

/* { dg-do compile } */
/* { dg-options "-gbtf -gprune-btf -dA" } */

/* We must have the full definitions of td1 and td3.  Neither are pruned.
   td2 will be skipped entirely, only because the only reference to
   it is through struct inner, which is pruned because inner itself
   is only used as a pointer member.

   In general, we must never get an anonymous FWD; the only FWD in this
   case will be for 'inner' */

/* Exactly 1 FWD for inner and no anonymous FWD.  */
/* { dg-final { scan-assembler-times "TYPE \[0-9\]+ BTF_KIND_FWD" 1 } } */
/* { dg-final { scan-assembler-not   "TYPE \[0-9\]+ BTF_KIND_FWD ''" } } */
/* { dg-final { scan-assembler       " BTF_KIND_FWD 'inner'" } } */

/* One anonymous struct for td1 and one anonymous union for td3.  */
/* { dg-final { scan-assembler-times "TYPE \[0-9\]+ BTF_KIND_STRUCT ''" 1 } } */
/* { dg-final { scan-assembler-times "TYPE \[0-9\]+ BTF_KIND_UNION ''" 1 } } */

/* The two remaining typedefs.  */
/* { dg-final { scan-assembler " BTF_KIND_TYPEDEF 'td1'" } } */
/* { dg-final { scan-assembler " BTF_KIND_TYPEDEF 'td3'" } } */

typedef struct {
  int x;
  char c;
} td1;

typedef struct {
  long l;
  char b[4];
} td2;

typedef union {
  long l;
  unsigned short s[2];
} td3;

struct inner {
  char a;
  td2 *ptd;
  long z;
};

struct A {
  td1 *pt;
  struct inner *in;
  unsigned long l[4];
};

struct A foo;

struct B {
  int x;
  td3 **ppptd3;
};

struct B bar;
