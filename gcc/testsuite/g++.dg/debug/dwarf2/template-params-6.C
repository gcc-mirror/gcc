// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-g -dA -fno-merge-debug-strings" }
// { dg-final { scan-assembler-times "DW_TAG_GNU_template_template_param" 2 } }
// { dg-final { scan-assembler-times "\"vector.0\"\[^\n\]*DW_AT_GNU_template_name" 1 } }
// { dg-final { scan-assembler-times ".ascii \"U.0\"\[^\n\]*DW_AT_name" 1 } }

template <class T>
struct vector_base
{
    T tab[3 + 1];
    static int get_sizeof_t()
    {
      return sizeof (tab);
    }
};

template <class T>
struct vector : public vector_base<T>
{
    static int get_sizeof_t()
    {
        return sizeof (T);
    }
    T member1;
    T member2;
};

template <template <class T> class U>
struct bar
{
  int foo()
  {
      return U<int>::get_sizeof_t ();
  }
};


int
foo_func ()
{
  bar<vector> b;
  return b.foo ();
}
