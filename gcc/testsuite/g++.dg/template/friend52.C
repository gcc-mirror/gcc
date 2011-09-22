// PR c++/50344
// { dg-options "" }

template <typename T> class C
{
   friend T;
   int i;
};

struct S
{
    int f()
    {
       C<const S> c;
       return c.i;
    }
};
