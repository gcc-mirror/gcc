// PR c++/70778

template <class KeyType>
struct Stuff
{
  template <KeyType, class>
  struct AddToFront;

  template <KeyType ToAdd, template<KeyType> class Holder, KeyType Indexs>
  struct AddToFront<ToAdd, Holder<Indexs> >
  {
  };
};

template <unsigned>
struct Holder {};

int main()
{
  Stuff<unsigned>::AddToFront<0, Holder<24> > t;
}
