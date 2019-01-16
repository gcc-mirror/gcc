int v1, v2;

/*
TEST_OUTPUT:
---
fail_compilation/fail13203.d(15): Error: alias fail13203.FA1!1.T conflicts with alias fail13203.FA1!1.T at fail_compilation/fail13203.d(14)
fail_compilation/fail13203.d(22): Error: template instance fail13203.FA1!1 error instantiating
fail_compilation/fail13203.d(20): Error: alias fail13203.FA2!1.T conflicts with alias fail13203.FA2!1.T at fail_compilation/fail13203.d(19)
fail_compilation/fail13203.d(23): Error: template instance fail13203.FA2!1 error instantiating
---
*/
template FA1(int b)
{
    alias T = int;
    static if (b) alias T = uint;
}
template FA2(int b)
{
    alias T = v1;
    static if (b) alias T = v2;
}
alias A1 = FA1!1;   // type is not overloadable
alias A2 = FA2!1;   // variable symbol is not overloadable

/*
TEST_OUTPUT:
---
fail_compilation/fail13203.d(36): Error: alias fail13203.FB1!1.T conflicts with alias fail13203.FB1!1.T at fail_compilation/fail13203.d(37)
fail_compilation/fail13203.d(44): Error: template instance fail13203.FB1!1 error instantiating
fail_compilation/fail13203.d(41): Error: alias fail13203.FB2!1.T conflicts with alias fail13203.FB2!1.T at fail_compilation/fail13203.d(42)
fail_compilation/fail13203.d(45): Error: template instance fail13203.FB2!1 error instantiating
---
*/
template FB1(int b)
{
    static if (b) alias T = uint;
    alias T = int;
}
template FB2(int b)
{
    static if (b) alias T = v2;
    alias T = v1;
}
alias B1 = FB1!1;
alias B2 = FB2!1;
