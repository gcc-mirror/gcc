// typecons.d
template RebindableCommon(T, U, This)
{
        union { U stripped; }

        void opAssign(T another)
        {
                stripped = cast() another;
        }

        this(T initializer)
        {
                opAssign(initializer);
        }
}


template Rebindable(T)
{
    static if (is(T == immutable U, U))
        struct Rebindable
        {
                mixin RebindableCommon!(T, U, Rebindable);
        }
}


