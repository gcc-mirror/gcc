#include <stdio.h>

int main()
{
    int* const savepos = sizeof(*savepos) ? 0 : 0;

    // code for the next line is left out!
    printf("size of thingy is %d\n", sizeof(*savepos));
}
