// REQUIRED_ARGS: -debug

// https://issues.dlang.org/show_bug.cgi?id=16578

string[string] opts;

void main()
{
    string arg;
    switch (arg)
    {
        case "-f": opts["fore"] = ""; break;
        debug { case "-throw": opts["throw"] = ""; break; }
        default:
    }
}
