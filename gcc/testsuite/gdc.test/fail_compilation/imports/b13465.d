module imports.b13465;

template isMaskField()
{
    import imports.b13465;
}

template isMatchingMaskField()
{
    enum isMatchingMaskField = { enum n = isMaskField!(); return n; }();

    /* Semantic analysis journey came from isMatchingMaskField!()
     *
     * TemplateInstance('isMaskField!T')->semantic()
     *  TemplateInstance('isMaskField!T')->semantic2() <---
     *          TemplateInstance::semantic() should run its semantic2() in function body.
     *   Import('import imports.ice1365a;')->semantic2()
     *    Module('imports.ice1365a')->semantic2()
     *     VarDeclaration('imports.ice1365a.isMatchingMaskField!().isMatchingMaskField')->semantic2() <---
     *          Cannot avoid this visiting, so we need to add a fix in VarDeclaration::semantic2().
     */
}
