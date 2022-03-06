// COMPILE_SEPARATELY:
// EXTRA_SOURCES: imports/bug10425.d

import imports.bug10425;

void main()
{
    auto ti = typeid(A!());
    /* Today, taking TypeInfo object address by using `typeid` always generates
     * the TypeInfo object on comdat section (done by TypeInfoDeclaration::toObjFile),
     * even if the associated struct belongs *non-root modules*.
     *
     * And, from 2.062, issue 7511 is implemented.
     * The attribute inference for member functions in instantiated struct may modify
     * their actual mangled names. Then TypeInfo object compiled in this module would
     * use wrong symbol names, to link non-template opEquals/opCmp/toHash/toString
     * member functions.
     *
     * To fix the issue, we should run semantic3 to calculate the correct symbol names
     * of the specific member functions.
     */
}
