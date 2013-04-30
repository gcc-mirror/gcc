// Origin: PR debug/45088
// { dg-do compile }
// { dg-options "-g -dA -fno-debug-types-section" }
// { dg-final { scan-assembler-times "\[^\n\r\]*\\(DIE\[^\n\r\]*DW_TAG_pointer_type\\)\[\n\r\]{1,2}\[^\n\r\]*DW_AT_byte_size\[\n\r\]{1,2}\[^\n\r\]*DW_AT_type" 4 } }

template<class T>
struct A
{
    virtual ~A(){}
};

struct B : public A<int>
{
    virtual ~B(){}
};

struct C : public B
{
    A<int>* a1;
};

int
main()
{
    C c;
    c.a1 = 0;
    return 0;
}

