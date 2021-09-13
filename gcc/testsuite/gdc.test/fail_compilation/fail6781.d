/*
TEST_OUTPUT:
---
fail_compilation/fail6781.d(9): Error: undefined identifier `some_error`
fail_compilation/fail6781.d(14): Error: template instance `fail6781.C6781.makeSortedIndices.bug6781!(greater)` error instantiating
---
*/
void bug6781(alias xxx)() {
    some_error;
}
struct C6781 {
    void makeSortedIndices() {
        int greater;
        bug6781!greater();
    }
}
