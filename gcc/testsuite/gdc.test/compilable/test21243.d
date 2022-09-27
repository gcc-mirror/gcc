// Parsing - expressions
auto a = auto ref (int x) => x;
auto b = auto ref (int x) { return x; };
auto c = function auto ref (int x) { return x; };
auto d = delegate auto ref (int x) { return x; };

// Parsing - aliases
alias e = auto ref (int x) => x;
alias f = auto ref (int x) { return x; };
alias g = function auto ref (int x) { return x; };
alias h = delegate auto ref (int x) { return x; };

// Semantic
void test()
{
    alias fun(alias x) = auto ref () => x;
    int n = 123;
    auto _ = fun!123();
    static assert(!__traits(compiles, &fun!123())); // rvalue
    fun!n() = 456; // lvalue
}
