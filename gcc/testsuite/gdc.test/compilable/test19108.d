// REQUIRED_ARGS: -ignore

// https://issues.dlang.org/show_bug.cgi?id=19108

pragma(unknown_global);
void main()
{
    pragma(unknown_local); // Error: unrecognized pragma
}
