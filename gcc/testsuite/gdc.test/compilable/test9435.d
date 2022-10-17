// EXTRA_FILES: test9434.d
import test9434;//expression;

enum TokenType { Dot }

template Tok(string type)
{
    enum Tok = TokenType.Dot;
}

template Semantic(T)
{
    invariant(){}
}

template Semantic(T) if (is(T == BinaryExp!(Tok!".")))
{
}
