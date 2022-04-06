/************************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/switches.d(105): Error: `case 2` not found
---
*/

#line 100
void test1(int i)
{
    switch (i)
    {
        case 1:
            goto case 2;
        defaut:
            break;
    }
}

/************************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/switches.d(205): Error: no `case` statement following `goto case;`
---
*/

#line 200
void test2(int i)
{
    switch (i)
    {
        case 1:
            goto case;
        defaut:
            break;
    }
}

/************************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/switches.d(302): Error: `switch` skips declaration of variable `switches.test3.j` at fail_compilation/switches.d(306)
---
*/

#line 300
void test3(int i)
{
    switch (i)
    {
        case 1:
        {
            int j;
        case 2:
            ++j;
            break;
        }
        default:
            break;
    }
}


/************************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/switches.d(404): Error: `switch` skips declaration of variable `switches.test.z` at fail_compilation/switches.d(406)
---
*/

#line 400
// https://issues.dlang.org/show_bug.cgi?id=18858

int test(int n)
{
    final switch(n)
    {
        int z = 5;
        enum e = 6;

        case 1:
            int y = 2;
            return y;
    }
}
