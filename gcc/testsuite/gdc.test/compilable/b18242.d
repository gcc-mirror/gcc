// REQUIRED_ARGS: -c
// PERMUTE_ARGS:
module object;

class Object { }

class TypeInfo { }
class TypeInfo_Class : TypeInfo
{
    version(D_LP64) { ubyte[136+16] _x; } else { ubyte[68+16] _x; }
}

class Throwable { }

int _d_run_main()
{
    try { } catch(Throwable e) { return 1; }
    return 0;
}
