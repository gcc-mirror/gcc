// Build don't link:

class bar {
public:
    bar() { rw = 0; }
    static const bar baz;
private:
    unsigned char rw;
};
char buf[4096];
void foo(char *uc)
{
    memcpy(buf,&bar::baz,sizeof(bar));
}
