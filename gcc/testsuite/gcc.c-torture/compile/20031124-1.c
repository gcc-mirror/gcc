/* PR 13143 */

int f (void *ptr)
{
    extern char const stop[];
    return ptr >= (void *) &stop;
}

