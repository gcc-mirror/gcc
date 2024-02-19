// { dg-do compile }
module object;

extern(C):
int printf(const char *, ...);

int main()
{
    printf("hello world\n");
    return 0;
}
