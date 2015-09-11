/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

struct A
{
};
template <class L, class R> A operator%(L, R);
template <class A0, class A1, class A2, class A3>
void make_tuple (A0 &, A1, A2, A3);
A
bar (int p1, char p2, int p3, double p4)
{
  A a;
  make_tuple (p1, p2, p3, p4);
  return "int; char; string; double; " % a;
}
A
foo (int p1, char p2, int p3, double p4)
{
  A b;
  make_tuple (p1, p2, p3, p4);
  return "int; char; string; double; " % b;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
