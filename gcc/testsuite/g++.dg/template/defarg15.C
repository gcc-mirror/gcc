// PR c++/54198

template <typename T> void
refIfNotNull (T* p1)
{
    p1->ref;
}
template <typename T> struct A
{
    A (T* p1)
    {
        refIfNotNull (p1);
    }
};
class B;
class C
{
    void getParent (A <B> = 0);
};
