// https://issues.dlang.org/show_bug.cgi?id=22646

static template logicalOr22646(T, const T name)
{
    enum bool ok = name.length < 3 || name[0..3] != "pad";
}
static template logicalAnd22646(T, const T name)
{
    enum bool ok = name.length >= 3 && name[0..3] == "pad";
}

bool runtime22646Or(T, const T name)()
{
    return name.length < 3 || name[0..3] != "pad";
}

// SCOPE.ctfe
static assert(logicalOr22646!(string, "x").ok == true);
static assert(logicalOr22646!(string, "foo").ok == true);
static assert(logicalOr22646!(string, "pad").ok == false);
static assert(logicalOr22646!(string, "pad123").ok == false);

static assert(logicalOr22646!(char[1], "x").ok == true);
static assert(logicalOr22646!(char[3], "foo").ok == true);
static assert(logicalOr22646!(char[3], "pad").ok == false);
static assert(logicalOr22646!(char[6], "pad123").ok == false);

static assert(logicalAnd22646!(string, "x").ok == false);
static assert(logicalAnd22646!(string, "foo").ok == false);
static assert(logicalAnd22646!(string, "pad").ok == true);
static assert(logicalAnd22646!(string, "pad123").ok == true);

static assert(logicalAnd22646!(char[1], "x").ok == false);
static assert(logicalAnd22646!(char[3], "foo").ok == false);
static assert(logicalAnd22646!(char[3], "pad").ok == true);
static assert(logicalAnd22646!(char[6], "pad123").ok == true);

// SCOPE.compile
enum char[1] x22646 = "x";
enum char[3] pad22646 = "pad";

static assert(__traits(compiles, x22646.length < 3 || x22646[0..3] != "pad") == true);
static assert(__traits(compiles, x22646.length >= 3 || x22646[0..3] == "pad") == true);
static assert(__traits(compiles, pad22646.length < 3 || pad22646[0..3] != "pad") == true);
static assert(__traits(compiles, pad22646.length >= 3 || pad22646[0..3] == "pad") == true);

// sc.intypeof
typeof(x22646.length < 3 || x22646[0..3] != "pad") typeof22646or1;
static assert(is(typeof(typeof22646or1) == bool));
typeof(pad22646.length < 3 || pad22646[0..3] != "pad") typeof22646or2;
static assert(is(typeof(typeof22646or2) == bool));

typeof(x22646.length >= 3 && x22646[0..3] == "pad") typeof22646and1;
static assert(is(typeof(typeof22646and1) == bool));
typeof(pad22646.length >= 3 && pad22646[0..3] == "pad") typeof22646and2;
static assert(is(typeof(typeof22646and2) == bool));

// No SCOPE flags
alias test22646a = runtime22646Or!(string, "x");
alias test22646b = runtime22646Or!(string, "foo");
alias test22646c = runtime22646Or!(string, "pad");
alias test22646d = runtime22646Or!(string, "pad123");

alias test22646e = runtime22646Or!(char[1], "x");
alias test22646f = runtime22646Or!(char[3], "foo");
alias test22646g = runtime22646Or!(char[3], "pad");
alias test22646h = runtime22646Or!(char[6], "pad123");
