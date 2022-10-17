// COMPILE_SEPARATELY:
// EXTRA_SOURCES: imports/c11447.d
// REQUIRED_ARGS: -w
// PERMUTE_ARGS: -allinst -debug -g

import imports.c11447;

void main()
{
    auto a = new A();
    TemplateInstancier().instanciateFromResolvedArgs(a);
}
