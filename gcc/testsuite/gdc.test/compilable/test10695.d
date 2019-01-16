// PERMUTE_ARGS:
module a;

void main()
{
    mixin("string mod1 = __MODULE__;");
    mixin("enum mod2 = __MODULE__;");
    static assert(mod2 == "a");
}
