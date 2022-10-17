// https://issues.dlang.org/show_bug.cgi?id=21806

void main()
{
    ubyte[16] arr;
    static assert(is(typeof(  fun(arr[])) == char));
    static assert(is(typeof(funtp(arr[])) == char));
    static assert(is(typeof(  bar(arr[])) == char));
}

// functions
char fun(ubyte[] arr) { return 'X'; }

int fun(ubyte[16] arr) { return 123; }

// function templates
char funtp()(ubyte[] arr) { return 'X'; }

int funtp(size_t N)(ubyte[N] arr) { return 123; }

// original case with 'in'
char bar()(in ubyte[] arr) { return 'X'; }

int bar(size_t N)(in ubyte[N] arr) { return 123; }
