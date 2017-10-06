// PR c++/82406

class a
{
public:
  template <typename b> void operator() (const b &);
};
void c () throw () __attribute__ ((__nonnull__));
void
d ()
{
  a e;
  e (c);
}
