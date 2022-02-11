/* TEST_OUTPUT:
---
fail_compilation/b15909.d(12): Error: duplicate `case 'a'` in `switch` statement
---
*/

void main()
{
    switch ('a')
    {
        case 'a':
        case 'a':
            break;
    }
}
