/* Test for a bug in 4.1 ivcanon code.  The loop ends up with two IV, both
   with a base of &num[1]. In the provess of converting the < condition to =
   we calculate (base1 - 1) - base0, which overflows and gives 0xffffffffu
   leading to a bogus loop iteration count.  */
#include <stdlib.h>

struct f
{
    int initial_offset;
    int can_eliminate;
    int can_eliminate_prev;
};


struct f num[2] = {{1, 3, 5}, {30, 50, 70}};
int x = 0;

int main()
{
    struct f *p;
    for (p = num; p < &num[1]; p++)
        {
            x += p->can_eliminate;
        }
    if (x != 3)
      abort();
    exit (0);
}
