// https://issues.dlang.org/show_bug.cgi?id=22198
// This test was in fail_compilation, however the change in the compiler has
// been reverted to make this code compilable again.
void main()
{
    int[10] a;
    auto b = a[1..12];
    auto c = a[4..3];
    auto d = a[0..$ + 1];
}
