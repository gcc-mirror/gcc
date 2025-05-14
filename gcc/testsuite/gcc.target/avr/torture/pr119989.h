const AS TYP some_data[] = { 1, 2, 3, 4, 5 };
const AS TYP *IP;

TYP DT, a, b;

__attribute__((noipa))
void do_test1 (void)
{
    DT = *IP;
    DT = *IP--;
}

__attribute__((noipa))
void do_test2 (void)
{
    DT = *IP;
    __asm volatile ("" ::: "memory"); // Prevents unwanted optimization
    DT = *IP--;
}

TYP difference(void)
{
    IP = &some_data[3];
    do_test1();
    a = DT;
    IP = &some_data[3];
    do_test2();
    b = DT;
    return a - b; // Expected: 0
}

int main (void)
{
    if (difference () != 0)
        __builtin_exit (__LINE__);
    return 0;
}
