module imports.issue18919b;

import core.stdc.stdio;

// Remove directories from paths. Used to make the output platform-independent.
string baseName(string path)
{
    foreach_reverse (i, char c; path)
    {
        if (c == '/' || c == '\\')
            return path[i + 1 .. $];
    }
    return path;
}
const(char)* baseName(const(char)* path)
{
    for (const(char)* ptr = path; *ptr; ptr++)
    {
        if (*ptr == '/' || *ptr == '\\')
            path = ptr + 1;
    }
    return path;
}

void func1(string file = __FILE__, size_t line = __LINE__,
    string func = __FUNCTION__,
    string pfunc = __PRETTY_FUNCTION__,
    string mod = __MODULE__)
{
    file = baseName(file);
    printf("%s: %.*s:%d %.*s %.*s %.*s\n", __FUNCTION__.ptr,
        cast(int) file.length, file.ptr, cast(int) line,
        cast(int) func.length, func.ptr,
        cast(int) pfunc.length, pfunc.ptr,
        cast(int) mod.length, mod.ptr);
}

// https://issues.dlang.org/show_bug.cgi?id=21211
void func2(const(char)* file = __FILE__.ptr, size_t line = __LINE__,
    const(char)* func = __FUNCTION__.ptr,
    const(char)* pfunc = __PRETTY_FUNCTION__.ptr,
    const(char)* mod = __MODULE__.ptr)
{
    file = baseName(file);
    printf("%s: %s:%d %s %s %s\n", __FUNCTION__.ptr,
        file, cast(int) line, func, pfunc, mod);
}

// https://issues.dlang.org/show_bug.cgi?id=18919
struct Loc3
{
    string file;
    size_t line;
    string func;
    string pfunc;
    string mod;
}
void func3(Loc3 loc = Loc3(__FILE__, __LINE__,
    __FUNCTION__, __PRETTY_FUNCTION__, __MODULE__))
{
    loc.file = baseName(loc.file);
    printf("%s: %.*s:%d %.*s %.*s %.*s\n", __FUNCTION__.ptr,
        cast(int) loc.file.length, loc.file.ptr, cast(int) loc.line,
        cast(int) loc.func.length, loc.func.ptr,
        cast(int) loc.pfunc.length, loc.pfunc.ptr,
        cast(int) loc.mod.length, loc.mod.ptr);
}
Loc3 defaultLoc3(string file = __FILE__, size_t line = __LINE__,
    string func = __FUNCTION__,
    string pfunc = __PRETTY_FUNCTION__,
    string mod = __MODULE__)
{
    return Loc3(file, line, func, pfunc, mod);
}
void func3b(Loc3 loc = defaultLoc3)
{
    loc.file = baseName(loc.file);
    printf("%s: %.*s:%d %.*s %.*s %.*s\n", __FUNCTION__.ptr,
        cast(int) loc.file.length, loc.file.ptr, cast(int) loc.line,
        cast(int) loc.func.length, loc.func.ptr,
        cast(int) loc.pfunc.length, loc.pfunc.ptr,
        cast(int) loc.mod.length, loc.mod.ptr);
}
enum Loc3Mixin = q{Loc3(__FILE__, __LINE__,
    __FUNCTION__, __PRETTY_FUNCTION__, __MODULE__)};
void func3c(Loc3 loc = mixin(Loc3Mixin))
{
    loc.file = baseName(loc.file);
    printf("%s: %.*s:%d %.*s %.*s %.*s\n", __FUNCTION__.ptr,
        cast(int) loc.file.length, loc.file.ptr, cast(int) loc.line,
        cast(int) loc.func.length, loc.func.ptr,
        cast(int) loc.pfunc.length, loc.pfunc.ptr,
        cast(int) loc.mod.length, loc.mod.ptr);
}
void func3d(Loc3* loc = new Loc3(__FILE__, __LINE__,
    __FUNCTION__, __PRETTY_FUNCTION__, __MODULE__))
{
    loc.file = baseName(loc.file);
    printf("%s: %.*s:%d %.*s %.*s %.*s\n", __FUNCTION__.ptr,
        cast(int) loc.file.length, loc.file.ptr, cast(int) loc.line,
        cast(int) loc.func.length, loc.func.ptr,
        cast(int) loc.pfunc.length, loc.pfunc.ptr,
        cast(int) loc.mod.length, loc.mod.ptr);
}

struct Loc4
{
    const(char)* file;
    size_t line;
    const(char)* func;
    const(char)* pfunc;
    const(char)* mod;
}
void func4(Loc4 loc = Loc4(__FILE__.ptr, __LINE__,
    __FUNCTION__.ptr, __PRETTY_FUNCTION__.ptr, __MODULE__.ptr))
{
    loc.file = baseName(loc.file);
    printf("%s: %s:%d %s %s %s\n", __FUNCTION__.ptr,
        loc.file, cast(int) loc.line,
        loc.func,
        loc.pfunc,
        loc.mod);
}
Loc4 defaultLoc4(const(char)* file = __FILE__.ptr, size_t line = __LINE__,
    const(char)* func = __FUNCTION__.ptr,
    const(char)* pfunc = __PRETTY_FUNCTION__.ptr,
    const(char)* mod = __MODULE__.ptr)
{
    return Loc4(file, line, func, pfunc, mod);
}
void func4b(Loc4 loc = defaultLoc4)
{
    loc.file = baseName(loc.file);
    printf("%s: %s:%d %s %s %s\n", __FUNCTION__.ptr,
        loc.file, cast(int) loc.line,
        loc.func,
        loc.pfunc,
        loc.mod);
}
enum Loc4Mixin = q{Loc4(__FILE__.ptr, __LINE__,
    __FUNCTION__.ptr, __PRETTY_FUNCTION__.ptr, __MODULE__.ptr)};
void func4c(Loc4 loc = mixin(Loc4Mixin))
{
    loc.file = baseName(loc.file);
    printf("%s: %s:%d %s %s %s\n", __FUNCTION__.ptr,
        loc.file, cast(int) loc.line,
        loc.func,
        loc.pfunc,
        loc.mod);
}
void func4d(Loc4* loc = new Loc4(__FILE__.ptr, __LINE__,
    __FUNCTION__.ptr, __PRETTY_FUNCTION__.ptr, __MODULE__.ptr))
{
    loc.file = baseName(loc.file);
    printf("%s: %s:%d %s %s %s\n", __FUNCTION__.ptr,
        loc.file, cast(int) loc.line,
        loc.func,
        loc.pfunc,
        loc.mod);
}

void func5(string file = baseName(__FILE__), int line = __LINE__,
    string func = __FUNCTION__,
    string pfunc = __PRETTY_FUNCTION__,
    string mod = __MODULE__)()
{
    printf("%s: %.*s:%d %.*s %.*s %.*s\n", __FUNCTION__.ptr,
        cast(int) file.length, file.ptr, line,
        cast(int) func.length, func.ptr,
        cast(int) pfunc.length, pfunc.ptr,
        cast(int) mod.length, mod.ptr);
}

void func6(string file = baseName(__FILE__), int line = __LINE__,
    const(char)* func = __FUNCTION__.ptr,
    const(char)* pfunc = __PRETTY_FUNCTION__.ptr,
    const(char)* mod = __MODULE__.ptr)()
{
    printf("%s: %.*s:%d %s %s %s\n", __FUNCTION__.ptr,
        cast(int) file.length, file.ptr, line, func, pfunc, mod);
}

void func7(int expr1 = 1000 +  __LINE__ * 2,
    string expr2 = "file=" ~ baseName(__FILE__) ~ " func=" ~ __FUNCTION__,
    int expr3 = __LINE__ > 5 ? 1 : 2)
{
    printf("%s: expr1=%d, %.*s, expr3=%d\n", __FUNCTION__.ptr, expr1, cast(int) expr2.length, expr2.ptr, expr3);
}

immutable string[2] constants = ["constant1", "constant2"];
void func8(int[] expr1 = [__LINE__, __LINE__ + 1000],
    int[string] expr2 = [baseName(__FILE__): __LINE__],
    string expr3 = constants[__LINE__ > 5],
    string expr4 = __FILE__[0 .. __FILE__.length - 2])
{
    expr4 = baseName(expr4);
    printf("%s: expr1=[", __FUNCTION__.ptr);
    foreach (i, x; expr1)
        printf("%d, ", x);
    printf("], expr2=[");
    foreach (k, v; expr2)
        printf("%.*s: %d, ", cast(int) k.length, k.ptr, v);
    printf("], expr3=%.*s", cast(int) expr3.length, expr3.ptr);
    printf(", expr4=%.*s\n", cast(int) expr4.length, expr4.ptr);
}

void func9(void function(string file = __FILE__, size_t line = __LINE__, string mod = __MODULE__)
    fp = (string file, size_t line, string mod)
    {
        file = baseName(file);
        printf("imports.issue18919b.func9.fp: %.*s:%d %.*s\n",
             cast(int) file.length, file.ptr, cast(int) line,
             cast(int) mod.length, mod.ptr);
    })
{
    fp();
}

void func10(string expr1 = mixin(() { return "\"expr1=" ~ __MODULE__ ~ "\""; } ()),
    string expr2 = mixin("\"expr2=" ~ __MODULE__ ~ "\""))
{
    printf("%s: %.*s, %.*s\n", __FUNCTION__.ptr,
        cast(int) expr1.length, expr1.ptr,
        cast(int) expr2.length, expr2.ptr);
}

template ctLoc3(string file, int line,
    string func, string pfunc, string mod)
{
    enum Loc3 ctLoc3 = Loc3(file, line, func, pfunc, mod);
}

void func11(Loc3 loc = ctLoc3!(baseName(__FILE__), __LINE__,
    __FUNCTION__, __PRETTY_FUNCTION__, __MODULE__))
{
    printf("%s: %.*s:%d %.*s %.*s %.*s\n", __FUNCTION__.ptr,
        cast(int) loc.file.length, loc.file.ptr, cast(int) loc.line,
        cast(int) loc.func.length, loc.func.ptr,
        cast(int) loc.pfunc.length, loc.pfunc.ptr,
        cast(int) loc.mod.length, loc.mod.ptr);
}

void func12(const(char)*[] args = [baseName(__FILE__.ptr),
    __FUNCTION__.ptr, __PRETTY_FUNCTION__.ptr, __MODULE__.ptr])
{
    printf("%s:", __FUNCTION__.ptr);
    foreach (arg; args)
        printf(" %s", arg);
    printf("\n");
}
