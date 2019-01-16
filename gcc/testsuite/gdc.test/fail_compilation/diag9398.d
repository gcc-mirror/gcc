/*
TEST_OUTPUT:
---
fail_compilation/diag9398.d(11): Error: incompatible types for ((f) : (s)): 'float' and 'string'
---
*/
void main()
{
    float f;
    string s;
    auto a = (true ? f : s);
}
