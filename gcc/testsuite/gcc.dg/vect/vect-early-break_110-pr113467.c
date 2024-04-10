/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_long_long } */

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"
#include <stdint.h>

typedef struct gcry_mpi *gcry_mpi_t;
struct gcry_mpi {
  int nlimbs;
  uint64_t *d;
};

long gcry_mpi_add_ui_up;
void gcry_mpi_add_ui(gcry_mpi_t w, gcry_mpi_t u, unsigned v) {
  gcry_mpi_add_ui_up = *w->d;
  if (u) {
    uint64_t *res_ptr = w->d, *s1_ptr = w->d;
    int s1_size = u->nlimbs;
    unsigned s2_limb = v, x = *s1_ptr++;
    s2_limb += x;
    *res_ptr++ = s2_limb;
    if (x)
      while (--s1_size) {
        x = *s1_ptr++ + 1;
        *res_ptr++ = x;
        if (x) {
          break;
        }
      }
  }
}

int main()
{
  check_vect ();

  static struct gcry_mpi sv;
  static uint64_t vals[] = {4294967288ULL, 191ULL,        4160749568ULL, 4294963263ULL,
                            127ULL,        4294950912ULL, 255ULL,        4294901760ULL,
                            534781951ULL,  33546240ULL,   4294967292ULL, 4294960127ULL,
                            4292872191ULL, 4294967295ULL, 4294443007ULL, 3ULL};
  gcry_mpi_t v = &sv;
  v->nlimbs = 16;
  v->d = vals;

  gcry_mpi_add_ui(v, v, 8);
  if (v->d[1] != 192)
    __builtin_abort();
}
