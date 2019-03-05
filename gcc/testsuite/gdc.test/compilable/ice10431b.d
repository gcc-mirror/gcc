struct X(alias Y)
{
}

struct A
{
    int[] data;
}

alias X!(A([])) X1;
alias X!(A([])) X2;
