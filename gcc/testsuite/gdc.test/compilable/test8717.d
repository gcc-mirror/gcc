module test8717;

struct SPR
{
private:
    enum e = 1;
    immutable int ii = 1;
    immutable static int sii = 1;
    static int sf() { return 1; }
    int f() const { return 1; }
}

static assert(SPR.e == 1);
//static assert(SPR.ii == 1);
static assert(SPR.sii == 1);
static assert(SPR.sf() == 1);
static assert(SPR.init.e == 1);
static assert(SPR.init.ii == 1);
static assert(SPR.init.sii == 1);
static assert(SPR.sf() == 1);
static assert(SPR.init.f() == 1);

static if(SPR.e != 1) { static assert(0); }
//static if(SPR.ii != 1) { static assert(0); }
static if(SPR.sii != 1) { static assert(0); }
static if(SPR.sf() != 1) { static assert(0); }
static if(SPR.init.e != 1) { static assert(0); }
static if(SPR.init.ii != 1) { static assert(0); }
static if(SPR.init.sii != 1) { static assert(0); }
static if(SPR.sf() != 1) { static assert(0); }
static if(SPR.init.f() != 1) { static assert(0); }

struct SPT
{
protected:
    enum e = 1;
    immutable int ii = 1;
    immutable static int sii = 1;
    static int sf() { return 1; }
    int f() const { return 1; }
}

static assert(SPT.e == 1);
//static assert(SPT.ii == 1);
static assert(SPT.sii == 1);
static assert(SPT.sf() == 1);
static assert(SPT.init.e == 1);
static assert(SPT.init.ii == 1);
static assert(SPT.init.sii == 1);
static assert(SPT.sf() == 1);
static assert(SPT.init.f() == 1);

static if(SPT.e != 1) { static assert(0); }
//static if(SPT.ii != 1) { static assert(0); }
static if(SPT.sii != 1) { static assert(0); }
static if(SPT.sf() != 1) { static assert(0); }
static if(SPT.init.e != 1) { static assert(0); }
static if(SPT.init.ii != 1) { static assert(0); }
static if(SPT.init.sii != 1) { static assert(0); }
static if(SPT.sf() != 1) { static assert(0); }
static if(SPT.init.f() != 1) { static assert(0); }

void main() { }
