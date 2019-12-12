void f(void delegate(int)) {}

class C
{
    int i;
    this()
    {
        f((a){});
        /* (a){} is a template lambda, so FuncExp::semantic -> TemplateDeclaration::semantic
         * will save the scope in TemplateDeclaration::scope with fieldinit. Later push/pop
         * of the scope for template lambda body semantics will violate the assertion in Scope::pop().
         */
    }
}
