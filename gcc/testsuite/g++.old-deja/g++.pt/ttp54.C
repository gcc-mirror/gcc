// { dg-do assemble  }

// Reported by Bruce Eckel <Bruce@EckelObjects.com>

// [temp.deduct.type]
// Make sure we treat <T> in the construct TT<T> as any type containing T.

template <class T> class C
{
};

template <class T, template <class> class TT> void f (TT<T *> &t)
{
}

int main ()
{
       C<char *> c;
       f(c);
}
