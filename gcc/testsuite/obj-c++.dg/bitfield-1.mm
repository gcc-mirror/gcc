/* Check if ObjC class layout follows the ABI (informally)
   set in the past.  ObjC structs must be laid out as if
   all ivars, including those inherited from superclasses,
   were defined at once (i.e., any padding introduced for
   superclasses should be removed).  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-Wpadded -Wabi" } */
/* { dg-do run } */

#include <objc/objc.h>
#include <objc/Object.h>
#include <stdlib.h>

#define CHECK_IF(expr) if(!(expr)) abort()

enum Enum { zero, one, two, three, four };

@interface Base: Object {
@public
  unsigned a: 2;
  int b: 3;
  enum Enum c: 4;
  unsigned d: 5;
} /* { dg-warning "padding struct size to alignment boundary" } */
@end

struct Base_0 { /* { dg-warning "padding struct size to alignment boundary" } */
  Class isa;
  unsigned a: 2;
  int b: 3;
  enum Enum c: 4;
  unsigned d: 5;
};

@interface Derived: Base {
@public
  signed e: 5;
  unsigned f: 4;
  enum Enum g: 3;
}
@end

struct Derived_0 {
  Class isa;
  unsigned a: 2;
  int b: 3;
  enum Enum c: 4;
  unsigned d: 5;
  signed e: 5;
  int f: 4;
  enum Enum g: 3;
};

@interface Leaf: Derived {
@public
  signed h: 2;
}
@end

struct Leaf_0 {
  Class isa;
  unsigned a: 2;
  int b: 3;
  enum Enum c: 4;
  unsigned d: 5;
  signed e: 5;
  unsigned f: 4;
  enum Enum g: 3;
  signed h: 2;
};
  
/* Note that the semicolon after @defs(...) is optional.  */

typedef struct { @defs(Base) } Base_t; /* { dg-warning "padding struct size to alignment boundary" } */
typedef struct { @defs(Derived); } Derived_t;
typedef struct { @defs(Leaf); } Leaf_t;

int main(void)
{
  struct Leaf_0 l_0;
  Leaf *l = (Leaf *)&l_0;
  Leaf_t *l_t = (Leaf_t *)&l_0;

  CHECK_IF(sizeof(Base_t) == sizeof(Base));
  CHECK_IF(sizeof(Derived_t) == sizeof(Derived));
  CHECK_IF(sizeof(Leaf_t) == sizeof(Leaf));

  CHECK_IF(sizeof(struct Base_0) == sizeof(Base));
  CHECK_IF(sizeof(struct Derived_0) == sizeof(Derived));
  CHECK_IF(sizeof(struct Leaf_0) == sizeof(Leaf));

  l_0.isa = (Class)0;
  l_0.a = 3;
  l_0.b = 0;
  l_0.c = three;
  l_0.d = 31;
  l_0.e = 0;
  l_0.f = 15;
  l_0.g = zero;
  l_0.h = -2;

  CHECK_IF(!l_t->isa);
  CHECK_IF(l->a == 3 && l_t->a == 3);
  CHECK_IF(!l->b && !l_t->b);
  CHECK_IF(l->c == three && l_t->c == three);
  CHECK_IF(l->d == 31 && l_t->d == 31);
  CHECK_IF(!l->e && !l_t->e);
  CHECK_IF(l->f == 15 && l_t->f == 15);
  CHECK_IF(l->g == zero && l_t->g == zero);
  CHECK_IF(l->h == -2 && l_t->h == -2);
  
  return 0;
}
