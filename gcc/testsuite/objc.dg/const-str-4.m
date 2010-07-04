/* Ensure that the preprocessor handles ObjC string constants gracefully. */
/* Author: Ziemowit Laski <zlaski@apple.com> */
/* { dg-options "-fconstant-string-class=MyString " } */ 
/* { dg-do run  } */

extern void abort(void);

@interface MyString
{
  void *isa;
  char *str;
  int len;
}
@end

#define kMyStringMacro1 "My String"
#define kMyStringMacro2 @"My String"

void *_MyStringClassReference;

@implementation MyString
@end

int main(void) {
  MyString* aString1 = @kMyStringMacro1;
  MyString* aString2 = kMyStringMacro2;
  if(aString1 != aString2) {
    abort();
  }
  return 0;
}
