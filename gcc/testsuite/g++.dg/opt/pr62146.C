/* PR rtl-optimization/62146 */
/* { dg-do compile } */
/* { dg-options "-O2 " } */
class F
{
public:
    virtual ~ F ();
};
template < class CL > class G:public F
{
    int *member_;
public:
    G ( int *b): member_ (0)
    {
    }
};

class D
{
public:
    template < class CL > void RegisterNonTagCallback (int,
            void (CL::
                  *p3) ())
    {
        InternalRegisterNonTag (p3 ? new G < CL > ( 0) : 0);
    } void InternalRegisterNonTag (F *);
};

void fn1 ();
class C1
{
    void  foo();
    class TokenType
    {
    public:
        void AddToken ()
        {
        }
    };
    C1::TokenType bar_t;
};
D a;
void C1::foo()
{
    if (&bar_t)
        fn1 ();
    for (int i = 0; i < sizeof 0; ++i)
        a.RegisterNonTagCallback (0, &TokenType::AddToken);
}

/* { dg-final { scan-assembler-not "mov.*_ZN2C19TokenType8AddTokenEv, .\\\(" } } */
