// { dg-do compile }

// Origin: jhbrown@bluefinrobotics.com

// PR c++/13635: ICE explicit specialization of member function template

template <class foo>
class bar {
public:
        template <class baz>
        int func(baz *x);
};

template <>
template <class baz>
int bar<double>::func(baz *x) { return 5;}

template <>
template <>
int bar<double>::func(int *x) { return 5;}
