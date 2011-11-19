// { dg-do compile }

struct Base
{
    void f();
    typedef int type;
    struct Type {};
    int i;
    static int j;
};

struct A : Base
{
    using Base::f; // { dg-message "previous declaration" }
    using Base::f; // { dg-error "redeclaration" }

    using Base::type; // { dg-message "previous declaration" }
    using Base::type; // { dg-error "redeclaration" }

    using Base::Type; // { dg-message "previous declaration" }
    using Base::Type; // { dg-error "redeclaration" }

    using Base::i; // { dg-message "previous declaration" }
    using Base::i; // { dg-error "redeclaration" }

    using Base::j; // { dg-message "previous declaration" }
    using Base::j; // { dg-error "redeclaration" }
};
