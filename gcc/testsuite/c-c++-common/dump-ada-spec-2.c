/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

struct S1 {
  struct {
    int i;
  } F;
};

struct S2 {
  union {
    int i;
  } F;
};

struct S3 {
  struct {
    int i;
  } F[2];
};

struct S4 {
  struct {
    struct S4 *next;
  } F;
};

struct S5 {
  struct {
    struct S5 *next;
  } F[2];
};

struct S6 {
  struct {
    struct S6 *next[2];
  } F;
};

struct S7 {
  struct {
    int i;
  } F1[2];
  struct {
    float f;
  } F2[2];
};

/* { dg-final { cleanup-ada-spec } } */
