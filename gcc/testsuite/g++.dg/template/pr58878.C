// PR c++/58878

// Template-members of non-template class
struct A
{
    template <typename t>    // { dg-error "shadows" }
        void f()
        {
            int t = 1;       // { dg-error "declaration" }
        }

    template <typename t>
        void g();
};

template <typename t>        // { dg-error "shadows" }
void A::g()
{
    int t = 2;               // { dg-error "declaration" }
}

// (Non-template) Members of template class
template <typename t>        // { dg-error "shadows" }
struct B
{
    void f()
    {
        int t = 3;           // { dg-error "declaration" }
    }

    void g();
};

template <typename t>        // { dg-error "shadows" }
void B<t>::g()
{
    int t = 4;               // { dg-error "declaration" }
}

// Template members of template class
template <typename t>        // { dg-error "shadows" }
struct C
{
    template <typename s>    // { dg-error "shadows" }
    void f()
    {
        int t = 5;           // { dg-error "declaration" }
        int s = 6;           // { dg-error "declaration" }
    }

    template <typename s>
    void g();
};

template <typename t>        // { dg-error "shadows" }
template <typename s>        // { dg-error "shadows" }
void C<t>::g()
{
    int t = 7;               // { dg-error "declaration" }
    int s = 8;               // { dg-error "declaration" }
}
