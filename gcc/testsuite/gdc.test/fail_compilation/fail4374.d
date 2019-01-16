/*
TEST_OUTPUT:
---
fail_compilation/fail4374.d(11): Error: terminating `;` required after do-while statement
---
*/

void main()
{
    do {} while(0)
}
