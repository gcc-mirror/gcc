/* { dg-do compile } */
/* { dg-options "" } */

#define N 64

struct f { int x; };
typedef struct f T;
typedef struct f T __attribute__((aligned (N)));
typedef struct f T __attribute__((aligned (N * 2)));
typedef struct f T __attribute__((aligned (N)));
typedef struct f T;

_Static_assert (_Alignof (T) == N * 2, "N * 2");

enum g { A = 1 };
typedef enum g S;
typedef enum g S __attribute__((aligned (N)));
typedef enum g S __attribute__((aligned (N * 2)));
typedef enum g S __attribute__((aligned (N)));
typedef enum g S;

_Static_assert (_Alignof (S) == N * 2, "N * 2");

