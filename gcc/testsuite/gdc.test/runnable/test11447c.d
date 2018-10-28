// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/c11447.d
// PERMUTE_ARGS: -allinst -w -debug -g

import imports.c11447;

void main()
{
    auto a = new A();
    TemplateInstancier().instanciateFromResolvedArgs(a);
}
