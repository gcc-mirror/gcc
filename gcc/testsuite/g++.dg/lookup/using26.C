// PR c++/26256
// { dg-do compile }

struct A
{
    double next;
};

struct B
{
private:
    int next; // { dg-message "private" }
};

struct C
{
    int next;
};

struct D : A, B, C
{
    using B::next; // { dg-error "context" }
    void f()
    {
	next = 12;
    }
};
