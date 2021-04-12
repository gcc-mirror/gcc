// https://issues.dlang.org/show_bug.cgi?id=19415

struct S
{
   int x;
   S foo() return { return S(x); }
   this(this) @disable;
}

S bar()
{
   S s;
   return s; // Error: struct `S` is not copyable because it is annotated with @disable
}
