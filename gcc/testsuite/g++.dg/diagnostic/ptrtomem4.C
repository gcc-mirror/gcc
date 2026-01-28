// PR c++/38612

struct Base {};
struct X;    // X derived from Base later but incomplete here
struct Y {}; // Y not derived from Base

int test1(int Base::* p2m, X* object)
{
    return object->*p2m; // { dg-error "int Base::.' incompatible with incomplete object type 'X'" }
}

struct X : Base
{
};

int test2(int Base::* p2m, X* object)
{
    return object->*p2m;        // OK
}

int test3(int Base::* p2m, Y* object)
{
    return object->*p2m;        // { dg-error "int Base::.' incompatible with object type 'Y' because 'Y' is not derived from 'Base'" }
}
