// PR c++/19397
// { dg-do compile }

template<typename> struct A
{
    typedef int ::template; // { dg-error "template" }
};
