struct S {
    static int foo()() { return 0; }
    static int foo()(int n) { return 1; }
    static int foo(string s) { return 2; }
    enum foo(int[] arr) = arr.length;
}

alias AliasSeq(T...) = T;

alias allFoos = AliasSeq!(__traits(getOverloads, S, "foo", true));

static assert(allFoos.length == 4);

static assert(allFoos[0]("") == 2);
static assert(allFoos[1]() == 0);
static assert(allFoos[2](1) == 1);
alias foo3 = allFoos[3];
static assert(foo3!([]) == 0);

static assert(S.foo() == 0);
static assert(S.foo(1) == 1);
static assert(S.foo("") == 2);
static assert(S.foo!([]) == 0);


alias fooFuns = AliasSeq!(__traits(getOverloads, S, "foo"));
static assert(fooFuns.length == 1);
static assert(fooFuns[0]("") == 2);