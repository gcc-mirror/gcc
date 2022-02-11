// REQUIRED_ARGS: -Icompilable/imports
// EXTRA_FILES: imports/pkg11847/mod11847.d imports/pkg11847/package.d
import pkg11847;
import pkg11847.mod11847;

void main() {
	static assert(pkg11847.func() == 1);
	static assert(pkg11847.mod11847.func() == 2);

    // This correctly won't compile.
    // Error: pkg11847.mod11847.func at imports/pkg11847/mod11847.d(3) conflicts with pkg11847.func at imports/pkg11847/package.d(3)
    static assert(!__traits(compiles, func()));

}
