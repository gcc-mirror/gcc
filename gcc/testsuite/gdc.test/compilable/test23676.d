// Issue 23676 - Static foreach hangs compilation for some time
// https://issues.dlang.org/show_bug.cgi?id=23676

void f()
{
    int i;
    void g(int I)()
    {
        static foreach(j; 0..11)
        {
            i++;
            g!j();
        }
    }
    g!0;
}
