// https://issues.dlang.org/show_bug.cgi?id=20318
// REQUIRED_ARGS: -dip1008 -profile=gc

void main()
{
    throw new Exception("msg");
}
