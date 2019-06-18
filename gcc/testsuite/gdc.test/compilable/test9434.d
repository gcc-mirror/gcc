// EXTRA_FILES: test9435.d
import test9435;//semantic;

template Visitors()
{
    mixin Semantic!(typeof(this));
}

class Node
{
    mixin Visitors;
}

class Expression : Node
{
}

class BinaryExp(TokenType op) : Expression
{
}
