// PR c++/47873
// { dg-do run }

struct Base
{
    virtual ~Base(){}

    virtual Base& This() { return *this; }
};


struct Ent : virtual Base
{
    void *m_Body;

    Ent& This() { return *this; }

    virtual Ent& body()
    {
        return This();
    }

};


struct Msg : virtual Ent
{
    Msg()
    {
        body();
    }

    Msg& This() { return *this; }
};

int main()
{
    Msg m;

    return 0;
}
