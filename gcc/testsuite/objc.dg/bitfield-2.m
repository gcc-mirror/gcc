/* Check if bitfield ivars are correctly @encode'd when
   the NeXT runtime is used.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-fnext-runtime -fsigned-char" } */
/* { dg-do run { target *-*-darwin* } } */

typedef struct objc_object { struct objc_class *class_pointer; } *id;

extern void abort(void);
extern int strcmp(const char *, const char *);

#define CHECK_IF(expr) if(!(expr)) abort();

@interface Base 
{
    struct objc_class *isa;
    int full;
    int full2: 32;
    int _refs: 8;
    int field2: 3;
    unsigned f3: 8;
    short cc;
    unsigned g: 16;
    int r2: 8;
    int r3: 8;
    int r4: 2;
    int r5: 8;
    char c;
}
@end

@interface Derived: Base
{
    char d;
    int _field3: 6;
}
@end

@implementation Base
@end

@implementation Derived
@end

int main(void) {
  const char *s1r = "{Base=#ib32b8b3b8sb16b8b8b2b8c}";
  const char *s1 = @encode(Base);
  const char *s2r = "{Derived=#ib32b8b3b8sb16b8b8b2b8ccb6}";
  const char *s2 = @encode(Derived);

  CHECK_IF(!strcmp(s1r, s1));
  CHECK_IF(!strcmp(s2r, s2));

  return 0;
}
