module imports.test9276decl;

import imports.test9276sem, imports.test9276visitors, imports.test9276util;

class Declaration
{
    mixin DownCastMethods!TemplateDecl;
}

class TemplateDecl : OverloadableDecl
{
    mixin Visitors;
}

