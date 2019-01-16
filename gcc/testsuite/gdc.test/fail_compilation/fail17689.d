/*
TEST_OUTPUT:
---
fail_compilation/fail17689.d(10): Error: undefined identifier `x`
---
*/
void main(){
    try{}
    finally int x=3;
    assert(x==3);
}
