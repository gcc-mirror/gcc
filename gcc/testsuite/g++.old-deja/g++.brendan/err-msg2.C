// { dg-do assemble  }
// GROUPS passed error-messages

typedef void (*pfv)(double, double);
extern "C" {
  typedef void (*pfv)(double, double); // { dg-error "" "" { xfail *-*-* } } conflicting linkage
}

