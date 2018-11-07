/*
TEST_OUTPUT:
---
fail_compilation/fail354.d(11): Error: template instance T!N template 'T' is not defined
fail_compilation/fail354.d(13): Error: template instance fail354.S!1 error instantiating
---
*/

struct S(int N)
{
    this(T!N) { }
}
alias S!1 M; 
