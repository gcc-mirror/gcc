// PRMS Id: 4342 (second testcase)
// Bug: g++ still can't deal with ambiguous inheritance in destructor calls.
// Build don't link:

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
{};	// gets bogus error - XFAIL *-*-*

struct ccSCCP : public ccVSTool
{};	// gets bogus error - XFAIL *-*-*

void foo ()
{
    ccSCCP* foo = new ccSCCP;
    delete foo;
}
