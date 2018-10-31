module imports.imp12242a;

public:
import imports.imp12242a1;  // std.string
import imports.imp12242a2;  // std.algorithm


private mixin template MixTmp(T, int x)
{
    template foo(U) if (is(U == T))
    {
        enum foo = x;
    }
}

mixin MixTmp!(int,  1);
mixin MixTmp!(long, 2);
