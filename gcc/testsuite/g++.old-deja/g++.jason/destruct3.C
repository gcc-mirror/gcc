// { dg-do assemble  }
// { dg-options "-w" }
// PRMS Id: 4342 (second testcase)
// Bug: g++ still can't deal with ambiguous inheritance in destructor calls.

struct ccUnwind 
{
    ccUnwind ();
    virtual ~ccUnwind ();
};

struct ccPersistent
{
    virtual void bar();
};

struct ccImpExp : public ccPersistent, public ccUnwind
{};

struct ccTool : public ccImpExp
{};

struct ccScreenTool : public ccTool
{};

struct ccVTool : public ccScreenTool
{};

struct ccScreenObjRep : public ccUnwind
{};

struct ccScreenObj : public ccScreenObjRep
{};

struct ccVSTool : public ccImpExp, public ccUnwind 
{};

struct ccSCCP : public ccVSTool
{};

void foo ()
{
    ccSCCP* foo = new ccSCCP;
    delete foo;
}
