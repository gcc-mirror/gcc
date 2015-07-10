// PR c++/30044

template <typename T1, typename T2, template <T2> class Comp, class Result = Comp<1> >
struct sort { };


template <typename Type, template <Type, Type> class Comp, class Result = Comp<1, 2> >
struct sort2 { };

template <typename Type, template <int, Type> class Comp, class Result = Comp<1, 2> >
struct sort3 { };

template <template <typename T1, typename T2, template <T2> class Comp, class Result = Comp<1> > class Foo>
struct sort4 { };
