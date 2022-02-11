// https://issues.dlang.org/show_bug.cgi?id=16578

void main()
{
    string[string] opts;
    switch (2)
    {
    case 0:
        opts["a"] = "";
        {
    case 1:
            break;
        }
    default:
    }
}
