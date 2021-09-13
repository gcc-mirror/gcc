/*
TEST_OUTPUT:
---
fail_compilation/fail225.d(15): Error: cannot implicitly convert expression `1` of type `int` to `immutable(char*)`
fail_compilation/fail225.d(15): Error: cannot implicitly convert expression `& ch` of type `char*` to `immutable(char*)`
---
*/
struct Struct { 
        char* chptr; 
}

void main()
{
        char ch = 'd';
        immutable Struct iStruct = {1, &ch};
}

