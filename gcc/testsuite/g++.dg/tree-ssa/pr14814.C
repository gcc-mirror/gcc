/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop2" } */

class YY { public:
  YY(const YY &v) { e[0] = v.e[0]; e[1] = v.e[1]; e[2] = v.e[2]; }
  double &y() { return e[1]; }
  double e[3];  };

class XX { public:
  YY direction() const { return v; }
  YY v;  };

int foo(XX& r) {
  if (r.direction().y() < 0.000001) return 0;
  return 1; }

/* { dg-final { scan-tree-dump-times "&this" 0 "forwprop2" { xfail *-*-* } } }*/
/* { dg-final { scan-tree-dump-times "&r" 0 "forwprop2" } } */
/* { dg-final { cleanup-tree-dump "forwprop2" } } */

