// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

int* hp;
int* jp;

void f (int *ip, int kp = hp - jp)
{
}
