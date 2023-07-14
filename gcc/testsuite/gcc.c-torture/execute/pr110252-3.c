
unsigned int a = 1387579096U;
void sinkandcheck(unsigned b) __attribute__((noipa));
void sinkandcheck(unsigned b)
{
        if (a != b)
        __builtin_abort();
}
int main() {
    a = 1 < (~a) ? 1 : (~a);
    sinkandcheck(1);
    return 0;
}
