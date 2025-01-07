// PERMUTE_ARGS:
// EXTRA_SOURCES: imports/testkwd_file.d
module testkeyword;
import imports.testkwd;

/****************************************/
// calee test

static assert(getCalleeFile()  == thatFile);
static assert(getCalleeLine()  == thatLine);
static assert(getCalleeMod()   == thatMod);
static assert(getCalleeFunc()  == thatFunc);
static assert(getCalleeFunc2() == thatFunc2);

void testCallee()
{
    static assert(getCalleeFile()  == thatFile);
    static assert(getCalleeLine()  == thatLine);
    static assert(getCalleeMod()   == thatMod);
    static assert(getCalleeFunc()  == thatFunc);
    static assert(getCalleeFunc2() == thatFunc2);
}

/****************************************/
// caller test

version(Windows) enum sep = "\\";  else enum sep = "/";

enum thisFile = "runnable"~sep~"testkeyword.d";
enum thisMod  = "testkeyword";

static assert(getFuncArgFile()  == thisFile);
static assert(getFuncArgLine()  == 33);
static assert(getFuncArgMod()   == thisMod);
static assert(getFuncArgFunc()  == "");
static assert(getFuncArgFunc2() == "");

static assert(getFuncTiargFile()  == thisFile);
static assert(getFuncTiargLine()  == 39);
static assert(getFuncTiargMod()   == thisMod);
static assert(getFuncTiargFunc()  == "");
static assert(getFuncTiargFunc2() == "");

static assert(getInstTiargFile!()  == thisFile);
static assert(getInstTiargLine!()  == 45);
static assert(getInstTiargMod!()   == thisMod);
static assert(getInstTiargFunc!()  == "");
static assert(getInstTiargFunc2!() == "");

void main(string[] args) nothrow
{
    enum thisFunc       = "testkeyword.main";
    enum thisFunc2 = "void testkeyword.main(string[] args) nothrow";

    static assert(getFuncArgFile()  == thisFile);
    static assert(getFuncArgLine()  == 56);
    static assert(getFuncArgMod()   == thisMod);
    static assert(getFuncArgFunc()  == thisFunc);
    static assert(getFuncArgFunc2() == thisFunc2);

    static assert(getFuncTiargFile()  == thisFile);
    static assert(getFuncTiargLine()  == 62);
    static assert(getFuncTiargMod()   == thisMod);
    static assert(getFuncTiargFunc()  == thisFunc);
    static assert(getFuncTiargFunc2() == thisFunc2);

    static assert(getInstTiargFile!()  == thisFile);
    static assert(getInstTiargLine!()  == 68);
    static assert(getInstTiargMod!()   == thisMod);
    static assert(getInstTiargFunc!()  == thisFunc);
    static assert(getInstTiargFunc2!() == thisFunc2);

    void nested(int x, float y) nothrow
    {
        enum thisFunc       = "testkeyword.main.nested";
        enum thisFunc2 = "void testkeyword.main.nested(int x, float y) nothrow";

        static assert(getFuncArgFile()  == thisFile);
        static assert(getFuncArgLine()  == 79);
        static assert(getFuncArgMod()   == thisMod);
        static assert(getFuncArgFunc()  == thisFunc);
        static assert(getFuncArgFunc2() == thisFunc2);

        static assert(getFuncTiargFile()  == thisFile);
        static assert(getFuncTiargLine()  == 85);
        static assert(getFuncTiargMod()   == thisMod);
        static assert(getFuncTiargFunc()  == thisFunc);
        static assert(getFuncTiargFunc2() == thisFunc2);

        static assert(getInstTiargFile!()  == thisFile);
        static assert(getInstTiargLine!()  == 91);
        static assert(getInstTiargMod!()   == thisMod);
        static assert(getInstTiargFunc!()  == thisFunc);
        static assert(getInstTiargFunc2!() == thisFunc2);
    }
    nested(1, 1.0);

    auto funcLiteral = (int x, int y)
    {
        enum thisFunc  = "testkeyword.main.__lambda_L98_C24";
        enum thisFunc2 = "testkeyword.main.__lambda_L98_C24(int x, int y)";

        static assert(getFuncArgFile()  == thisFile);
        static assert(getFuncArgLine()  == 104);
        static assert(getFuncArgMod()   == thisMod);
        static assert(getFuncArgFunc()  == thisFunc);
        static assert(getFuncArgFunc2() == thisFunc2);

        static assert(getFuncTiargFile()  == thisFile);
        static assert(getFuncTiargLine()  == 110);
        static assert(getFuncTiargMod()   == thisMod);
        static assert(getFuncTiargFunc()  == thisFunc);
        static assert(getFuncTiargFunc2() == thisFunc2);

        static assert(getInstTiargFile!()  == thisFile);
        static assert(getInstTiargLine!()  == 116);
        static assert(getInstTiargMod!()   == thisMod);
        static assert(getInstTiargFunc!()  == thisFunc);
        static assert(getInstTiargFunc2!() == thisFunc2);
    };
    funcLiteral(1, 2);

    static struct S
    {
        void func(string cs, T1, alias T2, T...)(int x) const
        {
            enum thisFunc       = `testkeyword.main.S.func!("foo", int, symbol, int[], float[]).func`;
            enum thisFunc2 = `void testkeyword.main.S.func!("foo", int, symbol, int[], float[]).func(int x) const`;

            static assert(getFuncArgFile()  == thisFile);
            static assert(getFuncArgLine()  == 131);
            static assert(getFuncArgMod()   == thisMod);
            static assert(getFuncArgFunc()  == thisFunc);
            static assert(getFuncArgFunc2() == thisFunc2);

            static assert(getFuncTiargFile()  == thisFile);
            static assert(getFuncTiargLine()  == 137);
            static assert(getFuncTiargMod()   == thisMod);
            static assert(getFuncTiargFunc()  == thisFunc);
            static assert(getFuncTiargFunc2() == thisFunc2);

            static assert(getInstTiargFile!()  == thisFile);
            static assert(getInstTiargLine!()  == 143);
            static assert(getInstTiargMod!()   == thisMod);
            static assert(getInstTiargFunc!()  == thisFunc);
            static assert(getInstTiargFunc2!() == thisFunc2);
        }
    }
    static int symbol;
    S s;
    s.func!("foo", int, symbol, int[], float[])(1);
}
