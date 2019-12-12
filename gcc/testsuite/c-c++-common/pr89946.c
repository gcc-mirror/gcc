/* PR c/89946 */

__attribute__((patchable_function_entry (-1))) void foo (void) {}	/* { dg-warning "'patchable_function_entry' attribute argument '-1' is not an integer constant" } */
__attribute__((patchable_function_entry (5, -5))) void bar (void) {}	/* { dg-warning "'patchable_function_entry' attribute argument '-5' is not an integer constant" } */
int i, j;
__attribute__((patchable_function_entry (i))) void baz (void) {}	/* { dg-warning "'patchable_function_entry' attribute argument 'i' is not an integer constant" } */
__attribute__((patchable_function_entry (2, j))) void qux (void) {}	/* { dg-warning "'patchable_function_entry' attribute argument 'j' is not an integer constant" } */
