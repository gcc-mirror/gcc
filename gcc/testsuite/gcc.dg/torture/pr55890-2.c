/* { dg-do compile } */
/* { dg-prune-output "conflicting types for built-in" } */

extern void *memcpy();
int main() { memcpy(); }

/* { dg-prune-output "\\\[-Wbuiltin-declaration-mismatch]" } */
