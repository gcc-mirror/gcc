struct foo { };
int f(int a, int b)
{
        if (b == 0)
                throw foo();
        return a / b;
}
int main()
{
        try {
                f(0, 0);
                return 0;
        } catch (foo x) {
                return 1;
        }
}
