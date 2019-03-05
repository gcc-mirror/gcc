/*
TEST_OUTPUT:
---
fail_compilation/fail4448.d(19): Error: label `L1` has no break
fail_compilation/fail4448.d(26):        called from here: bug4448()
fail_compilation/fail4448.d(26):        while evaluating: `static assert(bug4448() == 3)`
---
*/

int bug4448()
{
    int n=2;
    L1:{ switch(n)
    {
       case 5:
        return 7;
       default:
       n = 5;
       break L1;
    }
    int w = 7;
    }
    return 3;
}

static assert(bug4448()==3);
