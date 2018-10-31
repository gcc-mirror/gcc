module imports.test10736b;

version(A) import std.range;
else       import imports.test10736c;

void main()
{
    int[] arr = [0, 1, 2, 3];
    auto x = chunks(arr, 4);  // error

    import core.stdc.stdio;
    printf("success\n");
}
