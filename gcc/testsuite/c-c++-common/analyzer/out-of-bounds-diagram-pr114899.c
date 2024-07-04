/* Verify we don't ICE generating out-of-bounds diagram.  */

/* { dg-additional-options " -fsanitize=undefined -fdiagnostics-text-art-charset=unicode" } */

int * a() {
  int *b = (int *)__builtin_malloc(sizeof(int));
  int *c = b - 1;
  ++*c;
  return b;
}

/* We don't care about the exact diagram, just that we don't ICE.  */

/* { dg-allow-blank-lines-in-output 1 } */
/* { dg-prune-output ".*" } */
