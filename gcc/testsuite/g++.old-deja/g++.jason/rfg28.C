/* From 01/25/94 working paper (7.1.3):
 
        If, in a decl-specifier-seq containing the decl-specifier typedef,
        there is no type-specifier, or the only type-specifiers are cv-
        qualifiers, the typedef declaration is ill-formed.
*/
 
typedef foo;			// ERROR - invalid typedef
typedef const bar;		// ERROR - invalid typedef
