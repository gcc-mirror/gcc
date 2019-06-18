/*
https://issues.dlang.org/show_bug.cgi?id=21739

REQUIRED_ARGS: -debug
TEST_OUTPUT:
---
fail_compilation/debugCaseDeclaration.d(22): Error: undefined identifier `x`
fail_compilation/debugCaseDeclaration.d(33): Error: undefined identifier `y`
---
*/

void main()
{
    int i, k;
    switch (i)
    {
        case 0:
            int x;
            break;

        case 1:
            x = 1;
            break;

        case 2:
            int y;
            break;

        debug
        {
            case 3:
                k = 1; // Valid
                y = 1; // Invalid but accepted
                break;
        }

        default:
    }
}
