alias char* function() Func;

alias const char* function() CFunc;

void to(S)(S) { }

void foo(CFunc cFunc)
{
    to(cFunc());
}
