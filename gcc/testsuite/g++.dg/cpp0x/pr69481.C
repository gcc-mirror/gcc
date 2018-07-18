// PR c++/69481
// { dg-do compile { target c++11 } }

// ICE with canonical type verification

template <typename> struct Traits;

template <typename T>
struct Bob {
  using Loc = Traits<T>;
  using typename Loc::Thing;

  Thing Foo (); 
};

template <class V> struct tt
{
  using ut = tt<V>;
  ut Bob ();
};

template <class V>
tt<V> tt<V>::Bob ()
{
  return tt();
}
