// { dg-do assemble  }
// { dg-options "-w" }
// GROUPS passed visibility
// visibility file
// From: Alan Shepherd <a.shepherd@nexor.co.uk>
// Date:     Tue, 22 Jun 1993 14:53:23 +0100
// Subject:  bug with MI in gcc-2.4.5
// Message-ID: <9659.740757203@nexor.co.uk>

class A
{
    int a;

protected:

    virtual void State(int b)   { a = b; }

};

class B : public A
{
    char* foo;

public:

    B(const char*);
};

class C : public A
{
    char* foo2;

public:

    C(const char*);
};

class D : public B, public C
{
public:
  D();
protected:

    virtual void State(int a)
    {
        B::State(a);
        C::State(a);
    }
};



