// PR ipa/80212
// { dg-options "-O2 --param partial-inlining-entry-probability=403796683 -fno-early-inlining" }

struct b
{
  virtual b *c () const;
};
struct d : virtual b
{
};
struct e : d
{
  e *
  c () const
  {
  }
};
main () { e a; }
