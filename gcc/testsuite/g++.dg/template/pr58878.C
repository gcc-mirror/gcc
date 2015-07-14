// PR c++/58878

// Template-members of non-template class
struct A
{
    template <typename t>    // { dg-message "template parameter" }
        void f()
        {
            int t = 1;       // { dg-error "shadows" }
        }

    template <typename t>
        void g();
};

template <typename t>        // { dg-message "template parameter" }
void A::g()
{
    int t = 2;               // { dg-error "shadows" }
}

// (Non-template) Members of template class
template <typename t>        // { dg-message "template parameter" }
struct B
{
    void f()
    {
        int t = 3;           // { dg-error "shadows" }
    }

    void g();
};

template <typename t>        // { dg-message "template parameter" }
void B<t>::g()
{
    int t = 4;               // { dg-error "shadows" }
}

// Template members of template class
template <typename t>        // { dg-message "template parameter" }
struct C
{
    template <typename s>    // { dg-message "template parameter" }
    void f()
    {
        int t = 5;           // { dg-error "shadows" }
        int s = 6;           // { dg-error "shadows" }
    }

    template <typename s>
    void g();
};

template <typename t>        // { dg-message "template parameter" }
template <typename s>        // { dg-message "template parameter" }
void C<t>::g()
{
    int t = 7;               // { dg-error "shadows" }
    int s = 8;               // { dg-error "shadows" }
}
