/* Negative test case for constant string layout.  */
/* Contributed by Ziemowit Laski  <zlaski@apple.com>.  */

/* { dg-options "-fconstant-string-class=MyConstantString" } */
/* { dg-do compile } */

@interface MyBase {
  char p;
}
@end

@interface MyConstantString: MyBase {
  union {
    void     *u;
    unsigned char   *c;
  } _contents;
  char  _count;
}
@end

/* The NeXT runtime initializes the 'isa' pointer of string constants at
   compile time.  */
#ifdef __NEXT_RUNTIME__
extern void *_MyConstantStringClassReference;
#endif

MyConstantString *str = @"Hello"; /* { dg-error "interface .MyConstantString. does not have valid constant string layout" } */
