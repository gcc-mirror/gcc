// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=112270
// { dg-do compile }
class CPPNamespaceDeclaration { }
bool isNamespaceEqual (CPPNamespaceDeclaration a)
{
    return a ? true : isNamespaceEqual(a);
}
