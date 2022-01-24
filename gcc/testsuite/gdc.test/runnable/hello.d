// PERMUTE_ARGS:

extern(C) int printf(const char*, ...);

int main(char[][] args)
{
    printf("hello world\n");
    printf("args.length = %zd\n", args.length);
    for (int i = 0; i < args.length; i++)
        printf("args[%d] = '%.*s'\n", i, cast(int)args[i].length, args[i].ptr);
    return 0;
}
