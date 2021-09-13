// { dg-do compile { target { ilp32 || lp64 } } }

struct AddIn
{
    virtual ~AddIn() {}
    virtual void AddInCall()=0;
};

struct Base
{
    char b[32*1024*1024];   // Anything bigger than 16mb causes internal compiler error
    virtual ~Base() {}
};

struct Deriv : public Base,
                    public AddIn
{
    void AddInCall() {}
};

int main (int argc, char **argv)
{
    Deriv deriv;
    deriv.AddInCall();
    return 0;
}
