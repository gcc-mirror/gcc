// https://issues.dlang.org/show_bug.cgi?id=9029
enum NameOf(alias S) = S.stringof;

static assert(NameOf!int == "int");

enum BothMatch(alias S) = "alias";
enum BothMatch(T) = "type";

void foo9029() { }

struct Struct { }

static assert(BothMatch!int == "type");
static assert(BothMatch!(void function()) == "type");
static assert(BothMatch!BothMatch == "alias");
static assert(BothMatch!Struct == "type");
static assert(BothMatch!foo9029 == "alias");
static assert(BothMatch!5 == "alias");

// https://issues.dlang.org/show_bug.cgi?id=19884
mixin template genCtEvaluate()
{
    void evaluate(alias op)() { }
}
struct S
{
    mixin genCtEvaluate!() mixinEval;
    alias evaluate = mixinEval.evaluate;
    void evaluate() { }
}
alias List(Ops...) = Ops;
void main()
{
    S g;
    foreach (op; List!(0))
    {
        g.evaluate!op();
    }
}
