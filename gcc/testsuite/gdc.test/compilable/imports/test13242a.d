module imports.test13242a;

template expensiveArgs(alias v)
{
    pragma(msg, "a.expensiveArgs: ", v);
}

template expensiveTemplate(Args...)
{
    pragma(msg, "a.expensiveTemplate: ", Args[0]);
}

alias apiSym1 = expensiveTemplate!(1, expensiveArgs!(1));

alias apiSym2 = expensiveTemplate!(2, expensiveArgs!(2));

public import imports.test13242b : apiSym3;

void cheapFunc() {}
