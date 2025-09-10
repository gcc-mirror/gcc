__attribute__((noreturn)) void f1(void)
{
    while(true) {}
}
static void (*fptr)(void) = f1;
struct s1
{
    ~s1() {
        fptr();
    }
    void DoInner() {
        fptr();
    }
};

void f()
{
    s1 xxx;
    xxx.DoInner();
}
