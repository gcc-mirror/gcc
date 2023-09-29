// REQUIRED_ARGS:
// PERMUTE_ARGS:
class C
{
    void bug()
    {
        autoref!(true, C)(this);  // 'auto ref' becomes ref parameter
        autoref!(false, Object)(super); // 'auto ref' becomes non-ref parameter
    }
}

void autoref(bool result, T)(auto ref T t) { static assert(__traits(isRef, t) == result); }
