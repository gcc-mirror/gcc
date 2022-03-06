// https://issues.dlang.org/show_bug.cgi?id=16107

bool check()
{
    bool result = false;

    result |= false;
    if (result) goto ret;

    result |= false;
    if (result) {}

    ret: return true;
}
