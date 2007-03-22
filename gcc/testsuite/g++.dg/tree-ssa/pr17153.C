/* The alias analyzer was marking RETVAL non-addressable, but RETVAL
   is a special variable that's available across different functions.  */
void foo(const char*);

struct A {};

struct B : A
{
    B(){}
    B bar()
    {
        foo(__PRETTY_FUNCTION__);
        return B();
    }
};

B b=B().bar();
