/* Test alignment of stack variables.
 *
 * This test should be moved to "runnable" once DMD implements alignment of stack variables.
 */

void main()
{
    byte dummy;

    align(32) int align32;
    assert((cast(size_t)&align32 & cast(size_t)0b11111) == 0);
}
