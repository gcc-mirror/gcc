/* PR c/8439 */
/* Verify that GCC properly handles null increments. */

struct empty {
};

void foo(struct empty *p)
{
   p++;
}
