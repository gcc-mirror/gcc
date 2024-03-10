/*
TEST_OUTPUT:
---
fail_compilation/funcpostattr.d(11): Error: `deprecated` token is not allowed in postfix position
fail_compilation/funcpostattr.d(11): Error: `extern` token is not allowed in postfix position
fail_compilation/funcpostattr.d(15): Error: `static` token is not allowed in postfix position
fail_compilation/funcpostattr.d(15): Error: `ref` token is not allowed in postfix position
fail_compilation/funcpostattr.d(20): Error: `override` token is not allowed in postfix position
---
*/
void foo() deprecated extern;

void main() {
    int i;
    int foo() static ref => i;
}

class C
{
    void foo() override {}
}
