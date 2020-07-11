// https://issues.dlang.org/show_bug.cgi?id=17419


extern (C) int fooc();
alias aliasc = fooc;

static assert(__traits(getLinkage, fooc) == "C");
static assert(__traits(getLinkage, aliasc) == "C");

extern (D) int food();
extern (C++) int foocpp();
extern (Windows) int foow();
extern (Pascal) int foop();
extern (Objective-C) int fooobjc();
extern (System) int foos();

static assert(__traits(getLinkage, food) == "D");
static assert(__traits(getLinkage, foocpp) == "C++");
static assert(__traits(getLinkage, foow) == "Windows");
static assert(__traits(getLinkage, foop) == "Pascal");
static assert(__traits(getLinkage, fooobjc) == "Objective-C");
version (Windows)
    static assert(__traits(getLinkage, foos) == "Windows");
else
    static assert(__traits(getLinkage, foos) == "C");

extern (C) int global;
static assert(__traits(getLinkage, global) == "C");

static assert(__traits(getLinkage, typeof(fooc)) == "C");
static assert(__traits(getLinkage, typeof(&fooc)) == "C");

void bar()
{
    void nested() { }
    static assert(__traits(getLinkage, typeof(&nested)) == "D");
}

class FooD {}
interface FooDInterface {}
extern (C++) class FooCpp {}
extern (C++) struct FooCppStruct {}
extern (C++) interface FooCppInterface {}

static assert(__traits(getLinkage, FooD) == "D");
static assert(__traits(getLinkage, FooDInterface) == "D");
static assert(__traits(getLinkage, FooCpp) == "C++");
static assert(__traits(getLinkage, FooCppStruct) == "C++");
static assert(__traits(getLinkage, FooCppInterface) == "C++");

version (D_ObjectiveC)
{
    extern (Objective-C) interface FooObjC {}
    static assert(__traits(getLinkage, FooObjC) == "Objective-C");
}
