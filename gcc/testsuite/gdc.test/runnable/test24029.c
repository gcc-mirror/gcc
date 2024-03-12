// https://issues.dlang.org/show_bug.cgi?id=24029

int x = 0;
int y = 0;

void a()
{
    (__extension__ ({ x += 2; })); // test.a.__dgliteral1
}

void b()
{
    (__extension__ ({ y += 1; })); // test.b.__dgliteral1
}

int main(void)
{
    a();
    b();
    __check(x == 2);
    __check(y == 1);
    return 0;
}
