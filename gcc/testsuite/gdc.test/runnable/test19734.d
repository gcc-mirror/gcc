// https://issues.dlang.org/show_bug.cgi?id=19734
// REQUIRED_ARGS: -main

class C19734
{
    import core.stdc.stdarg;

    extern
    {
        // Invalid 'this' parameter because of applied 'extern' storage class.
        void testin(typeof(this) p)
            in(this is p)
        {
        }

        // Undefined reference to __result.
        int testout()
            out(; __result == 2)
        {
            return 2;
        }

        // Undefined reference to var.
        int testlocal()
        {
            int var;
            return var + 2;
        }

        // Variable _argptr cannot have initializer.
        int testvarargs(...)
        {
            return 0;
        }
    }
}
