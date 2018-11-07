// https://issues.dlang.org/show_bug.cgi?id=16303

void yayf(void function(int*) fp);
void yayd(void delegate(int*) dg);

void bar()
{
    void function(const(int)* p) fp;
    yayf(fp);                   // should be good but produces error

    void delegate(const(int)* p) dg;
    yayd(dg);                   // should be good but produces error
}
