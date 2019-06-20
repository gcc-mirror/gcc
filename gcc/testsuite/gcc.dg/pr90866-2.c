/* PR tree-optimization/90866 - ICE in fold_binary_loc, at fold-const.c:9827
   { dg-do compile  }
   { dg-require-effective-target global_constructor }
   { dg-options "-O2 -fsanitize=thread" }
 */

typedef enum { a } b;
typedef struct {
  int c[0];
} d;
typedef struct {
  int *data;
} e;
typedef struct {
  e buffer;
} f;
int g, h;

int i(f *j, d *k, b l, int m) {
  if (l)
    if (m) {
      h = j->buffer.data[0];
      k->c[g] = k->c[g] * 8;
    }
  return 0;
}
