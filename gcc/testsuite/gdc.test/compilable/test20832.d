// PERMUTE_ARGS: -preview=dip1000
// https://issues.dlang.org/show_bug.cgi?id=20823

void boo(T)(  scope   void delegate(T[] data) fun) {}
void goo(T)(/+scope+/ void delegate(T[] data) fun) {}

void main()
{
    void Execute(int[] data) {}
    goo(&Execute);
    boo(&Execute);
}
