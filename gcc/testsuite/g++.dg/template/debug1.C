// PR c++/40274
// { dg-options "-g" }

template <class T> struct valuelist_types
{
 struct null { };
 template <T V, class next=null> struct list { };
};

template <unsigned D> void foo()
{
 typename valuelist_types<unsigned>::template list<D> v;
}

void bar()
{
 valuelist_types<unsigned>::list<2> v;
}
