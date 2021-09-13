/* { dg-do assemble } */
typedef int __attribute((vector_size(16))) V;
V f(void) {
  return (V){ (int)f, (int)f, (int)f, (int)f };
}
