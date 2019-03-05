
// REQUIRED_ARGS: -O -profile -inline

struct Range {
    private string s;
    char charAt(int unused1) { return s[0]; }
}

bool count(Range* r, int* unused2)
{
    *unused2 = 0;
    int unused3;
    char c = r.charAt(0);
    return true;
}
