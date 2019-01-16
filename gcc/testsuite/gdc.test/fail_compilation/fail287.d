/*
TEST_OUTPUT:
---
fail_compilation/fail287.d(14): Error: had 299 cases which is more than 256 cases in case range
---
*/


void main()
{
    int i = 2;
    switch (i)
    {
        case 1: .. case 300:
            i = 5;
            break;
    }
    if (i != 5)
        assert(0);
}
