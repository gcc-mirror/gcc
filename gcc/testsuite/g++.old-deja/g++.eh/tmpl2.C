// { dg-do assemble { xfail i*86-*-linux* } }
// { dg-options "-O" }

// Posted by H. J. Lu <hjl@lucon.org>

template<class T>
class FixSeq
{
public:
    void append(const T&);
};
class foo
{
public:  
    void setupIR();
};
typedef FixSeq<foo *> bar;
extern void dummy (foo *);
void *
foobar (bar &x, foo *p)
{
    try
    {
        p -> setupIR();
    }
    catch(...)
    {
        dummy (p);
    }
    x.append(p);
    return p;
}
