// Test that postfix attributes only apply to a single declared object.
// (decl_attributes used to chain them onto the end of the prefix attributes,
// which caused them to apply to other declarations as well.)
// Origin: Joseph Myers <jsm28@cam.ac.uk>.
// Build don't link:
void __attribute__((__noreturn__)) foo (const char *, ...) __attribute__((__format__(__printf__, 1, 2))), bar (void);
