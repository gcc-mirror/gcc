// https://issues.dlang.org/show_bug.cgi?id=17373
interface Foo { void visit (int); }
interface Bar { void visit(double); }
interface FooBar : Foo, Bar {}
static assert(__traits(getOverloads, FooBar, "visit").length == 2);

interface Fbar { void visit(char); void visit(double); }
interface Triple : Foo, Bar, Fbar {}
static assert(__traits(getOverloads, Triple, "visit").length == 3);

interface InheritanceMadness : FooBar, Triple {}
static assert(__traits(getOverloads, Triple, "visit").length == 3);

interface Simple
{
    int square(int);
    real square(real);
}
static assert(__traits(getOverloads, Simple, "square").length == 2);

// https://issues.dlang.org/show_bug.cgi?id=19064
interface InputStream {}
interface OutputStream{}
interface Stream : InputStream, OutputStream{}
interface ConnectionStream : Stream
{
    @property bool connected() const;
    void close();
}

static assert(__traits(getOverloads, ConnectionStream, "connected").stringof == "tuple(connected)");
static assert(__traits(getOverloads, ConnectionStream, "close").stringof == "tuple(close)");
