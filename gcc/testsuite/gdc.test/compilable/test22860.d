// https://issues.dlang.org/show_bug.cgi?id=22860
class C1
{
    SumType!(C1, C2) field;
}

class C2
{
    SumType!(SumType!(C1, C2)) field;
}

alias AliasSeq(TList...) = TList;

template allSatisfy(alias F, T...)
{
    static foreach (Ti; T)
    {
        static if (!F!Ti)
            enum allSatisfy = false;
    }
}

struct This {}

enum isAssignableTo(T) = isAssignable!T;
enum isHashable(T) = __traits(compiles, { T.init; });

struct SumType(Types...)
{
    alias Types = AliasSeq!(ReplaceTypeUnless!(isSumTypeInstance, This, typeof(this), TemplateArgsOf!SumType));

    static foreach (T; Types)
    {
        static if (isAssignableTo!T)
        {
        }
    }

    static if (allSatisfy!(isAssignableTo, Types))
    {
    }

    static if (allSatisfy!(isHashable, Types))
        size_t toHash;
}

bool isSumTypeInstance;

alias TemplateArgsOf(T : Base!Args, alias Base, Args...) = Args;
enum isAssignable(Lhs, Rhs = Lhs) = isRvalueAssignable!(Lhs, Rhs) ;
enum isRvalueAssignable(Lhs, Rhs ) = __traits(compiles, { lvalueOf!Lhs = Rhs; });

struct __InoutWorkaroundStruct{}
T lvalueOf(T)(__InoutWorkaroundStruct );

template ReplaceTypeUnless(alias pred, From, To, T...)
{
    static if (T.length == 1)
        alias ReplaceTypeUnless = T;
    static if (T.length > 1)
        alias ReplaceTypeUnless = AliasSeq!(ReplaceTypeUnless!(pred, From, To, T[1 ]));
}
