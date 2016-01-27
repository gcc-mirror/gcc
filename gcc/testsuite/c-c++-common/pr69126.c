/* { dg-options "-Wunused-variable" } */

#pragma GCC diagnostic push
int f()
{
    _Pragma("GCC diagnostic ignored \"-Wunused-variable\"")
    int x;
    return 0;
}
#pragma GCC diagnostic pop

#pragma GCC diagnostic push
#define MACRO \
    _Pragma("GCC diagnostic ignored \"-Wunused-variable\"") \
    int x;

int g()
{
    MACRO;
    return 0;
}
#pragma GCC diagnostic pop
