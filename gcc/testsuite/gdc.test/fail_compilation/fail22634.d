/*
TEST_OUTPUT:
---
fail_compilation/fail22634.d(9): Error: more than 65535 symbols with name `i` generated
---
*/
void main()
{
    static foreach(i; 0..65537)
    {
    }
}
