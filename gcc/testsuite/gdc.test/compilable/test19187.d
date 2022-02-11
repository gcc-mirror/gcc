// EXTRA_FILES: imports/test19187.d
import imports.test19187;
void main()
{
    enum test = __traits(compiles, imports.test19187.foo);
}
