// https://issues.dlang.org/show_bug.cgi?id=22390

int main()
{
    noreturn[] empty;
    assert(empty == empty);
    return 0;
}
