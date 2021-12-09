module imports.Other; // makes no difference if removed
import Same;
import core.stdc.stdio;

class Other : Same.Same
{
    this()
    {
        printf("other\n");
    }
}

int main()
{
    new Other;
    return 0;
}
