/*
TEST_OUTPUT:
---
fail_compilation/diag12640.d(14): Error: undefined identifier `asdf`
fail_compilation/diag12640.d(23): Error: undefined identifier `asdf`
---
*/

void main()
{
    switch (1)
    {
        case 0:
            asdf;
            break;

        default:
    }

    switch (1)
    {
        default:
            asdf;
            break;

        case 0:
    }

}
