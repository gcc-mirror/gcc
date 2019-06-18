
struct TypeObj
{
    alias This = typeof(this);

    mixin template MixinTempl()
    {
        int value;
    }
}

ref TypeObj Obj()
{
    static TypeObj a;
    return a;
}

void func()
{
    mixin Obj.This.MixinTempl; // ok
    mixin Obj.MixinTempl;      // fixed: "MixinTempl!()" is not defined
}
