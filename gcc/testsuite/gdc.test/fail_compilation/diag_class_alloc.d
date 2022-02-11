/*
TEST_OUTPUT:
---
fail_compilation/diag_class_alloc.d(15): Error: `new` allocator must be annotated with `@disabled`
fail_compilation/diag_class_alloc.d(16): Deprecation: `new` allocator with non-empty parameter list is deprecated
fail_compilation/diag_class_alloc.d(16): Deprecation: `new` allocator with function definition is deprecated
---
*/

// This test exists to ensure class allocators and deallocators emit an appropriate error message.
// This test can be deleted when class allocators and deallocators are removed from the language.

class C
{
    new(size_t size)         // error message
    {
        return malloc(size);
    }
}
