module imports.pkg.a314; // sub package

package(imports) static import imports.c314;
package(imports) import renamed = imports.c314;
package(imports) import imports.c314 : bug;
