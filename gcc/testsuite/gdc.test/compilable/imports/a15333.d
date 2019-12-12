module imports.a15333;

import ice15333;

struct StatementVisitor
{
    void visit()
    {
        int location;
        alias IR = IdentifierResolver!((e){ location = 0; });
    }
}
