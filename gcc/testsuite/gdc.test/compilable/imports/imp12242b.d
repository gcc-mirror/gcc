module imports.imp12242b;

public:
import imports.imp12242b1;  // std.string
import imports.imp12242b2;  // std.algorithm


private mixin template MixTmp(T, int x)
{
    template foo(U) if (is(U == T))
    {
        enum foo = x;
    }
}

mixin MixTmp!(float, 3);
mixin MixTmp!(real,  4);
