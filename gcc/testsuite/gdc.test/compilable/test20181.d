module test20181;

struct InversionList
{
    ubyte[] byCodepoint() { return null; }
}

void main()
{
    static foreach (ch; InversionList().byCodepoint) { }
}
