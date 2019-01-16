module imports.a13465;

template isMaskField()
{
    import imports.a13465;
}

template isMatchingMaskField()
{
    enum isMatchingMaskField = isMaskField!();

    /* Semantic analysis journey came from isMatchingMaskField!()
     *
     * TemplateInstance('isMaskField!T')->semantic()
     *  TemplateInstance('isMaskField!T')->semantic2() <---
     *          TemplateInstance::semantic() will run its semantic2() always.
     *   Import('import imports.ice1365a;')->semantic2()
     *    Module('imports.ice1365a')->semantic2()
     *     VarDeclaration('imports.ice1365a.isMatchingMaskField!().isMatchingMaskField')->semantic2() <---
     *          The type field is yet NULL during type inference, then ICE happens.
     */
}
