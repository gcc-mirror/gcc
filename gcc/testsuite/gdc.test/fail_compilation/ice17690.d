/*
TEST_OUTPUT:
---
fail_compilation/ice17690.d(9): Error: undefined identifier `x`
---
*/
void main(){
    scope(exit) int x=3;
    assert(x==3);
}
