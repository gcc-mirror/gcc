/*
TEST_OUTPUT:
---
fail_compilation/goto3.d(1010): Error: case cannot be in different `try` block level from `switch`
fail_compilation/goto3.d(1012): Error: default cannot be in different `try` block level from `switch`
---
 */


void foo();
void bar();

#line 1000

void test1()
{
    int i;
    switch (i)
    {
        case 1:
            try
            {
                foo();
        case 2:
                {   }
        default:
                {   }
            }
            finally
            {
                bar();
            }
            break;
    }
}


