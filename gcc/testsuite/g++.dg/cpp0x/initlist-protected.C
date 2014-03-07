// PR c++/54325
// { dg-do compile { target c++11 } }

class base
{
    protected:
        base()
        {}
};

class derived : public base
{
    public:
        derived()
            : base{} // <-- Note the c++11 curly brace syntax
        {}
};

int main()
{
    derived d1;
    return 0;
}
