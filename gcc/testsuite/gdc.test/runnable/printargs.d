// PERMUTE_ARGS:
// EXECUTE_ARGS: A B C

extern(C) int printf(const char*, ...);

int main(char args[][])
{
    int i;

    for (i = 0; i < args.length; i++)
        printf("args[%d] = '%.*s'\n", i, args[i].length, args[i].ptr);

    assert(args[1] == "A");
    assert(args[2] == "B");
    assert(args[3] == "C");

    return 0;
}
