struct A { typedef int I; };
int main(void)
{
        int * p;
        p->A::I::~I();
}
