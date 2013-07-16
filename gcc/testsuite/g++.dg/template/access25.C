// PR c++/56359

typedef int (*InvocationCallback) (const int &);

template < typename target_t >
void SetPrototypeMethod (target_t, const char *, InvocationCallback);

class A
{
    void Initialize ();
protected:
    static int Stop (const int &);
    void Stop ();  // comment out to make the bug disappear.
};

void
A::Initialize ()
{
    SetPrototypeMethod (0, "stop", A::Stop);
}
