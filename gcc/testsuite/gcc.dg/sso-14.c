/* PR c/100920 */
/* Testcase by George Thopas <george.thopas@gmail.com> */

/* { dg-do compile } */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define REV_ENDIANNESS __attribute__((scalar_storage_order("big-endian")))
#else
#define REV_ENDIANNESS __attribute__((scalar_storage_order("little-endian")))
#endif

struct s_1 {
    int val;
} REV_ENDIANNESS;

typedef struct s_1 t_1;

struct s_2 {
    char val;
} REV_ENDIANNESS;

typedef struct s_2 t_2;

struct s12 {
    t_1 a[1];
    t_2 b[1]; 
} REV_ENDIANNESS;

typedef struct s12 t_s12;

union u12 {
    t_1 a[1];
    t_2 b[1];
} REV_ENDIANNESS;

typedef union u12 t_u12;

int main(void)
{
  t_s12 *msg1 = __builtin_alloca(10);
  t_u12 *msg2 = __builtin_alloca(10);
  int same;

  msg1 = malloc (sizeof (t_s12));
  msg2 = malloc (sizeof (t_s12));

  memset (msg1, 0, sizeof (t_s12));
  memcpy (msg2, msg1, sizeof (t_s12));
  same = memcmp (msg1, msg2, sizeof (t_s12));

  return 0;
}
