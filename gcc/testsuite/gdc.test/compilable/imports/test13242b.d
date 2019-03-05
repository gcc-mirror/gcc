module imports.test13242b;

template expensiveArgs(alias v)
{
    pragma(msg, "b.expensiveArgs: ", v);
}

template expensiveTemplate(Args...)
{
    pragma(msg, "b.expensiveTemplate: ", Args[0]);
}

alias apiSym3 = expensiveTemplate!(3, expensiveArgs!(3));
