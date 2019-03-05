// REQUIRED_ARGS: -O
version (X86_64):
alias vec = __vector(int[4]);

vec binop(vec a)
{
    vec b = a;
    return b;
}
