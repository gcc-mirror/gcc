/*
PERMUTE_ARGS:
RUN_OUTPUT:
---
hello world
---
*/

extern(C) int printf(const char *, ...);

int main(char[][] args)
{
    printf("hello world\n");

    return 0;
}
