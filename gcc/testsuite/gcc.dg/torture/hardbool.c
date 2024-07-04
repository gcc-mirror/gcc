/* { dg-do run } */

#include <assert.h>

#ifndef basetype
#define basetype char
#endif

#ifndef falseval
#define falseval 0
#endif

#ifndef trueval
#define trueval ~falseval
#endif

/* hardbool may be #defined so as to drop parms in other tests.  */
typedef basetype __attribute__ ((hardbool (falseval, trueval))) hbool;

typedef unsigned char __attribute__ ((__hardbool__ (1, 0))) zbool;

struct hs {
  hbool a[2];
  hbool x:2;
  hbool y:5;
  zbool z:1;
};

hbool var = 0;

struct hs x = { { 1, 0 }, 2, 0, 2 };

int f(hbool v) {
  return !v;
}

int g(int i) {
  return f(i);
}

hbool h(hbool x) {
  return x;
}

hbool h2(hbool x) {
  return h(x);
}

int hsx(struct hs v) {
  return v.x;
}

int ghs(hbool s) {
  struct hs v = { {s, !s}, s, !s, s };
  return hsx (v);
}

int t = (hbool)2;

void check_pfalse (hbool *p)
{
  assert (!*p);
  assert (*(basetype*)p == (basetype)falseval);
  assert (!(int)(hbool)*p);
}

void check_ptrue (hbool *p)
{
  assert (*p);
  assert (*(basetype*)p == (basetype)trueval);
  assert ((int)(hbool)*p);
}

void check_vfalse (hbool v)
{
  check_pfalse (&v);
}

void check_vtrue (hbool v)
{
  check_ptrue (&v);
}

int main () {
  check_pfalse (&var);
  var = !(int)(hbool)(_Bool)var;
  check_ptrue (&var);
  var = (zbool)var;
  check_ptrue (&var);

  check_ptrue (&x.a[0]);
  check_pfalse (&x.a[1]);
  check_vtrue (x.x);
  check_vfalse (x.y);
  check_vtrue (x.z);

  check_vtrue (t);

  check_vtrue (var && t);
  check_vfalse (!var || x.y);

  check_vfalse (f (2));
  check_vfalse (f (1));
  check_vtrue (f (0));

  check_vfalse (g (2));
  check_vfalse (g (1));
  check_vtrue (g (0));

  check_vtrue (h (2));
  check_vtrue (h (1));
  check_vfalse (h (0));

  check_vtrue (h2 (2));
  check_vtrue (h2 (1));
  check_vfalse (h2 (0));
}

