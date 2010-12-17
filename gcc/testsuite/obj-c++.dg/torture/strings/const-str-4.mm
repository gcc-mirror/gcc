/* Ensure that the preprocessor handles ObjC string constants gracefully. */
/* Author: Ziemowit Laski <zlaski@apple.com> */

/* { dg-do run  { target *-*-darwin* } } */
/* { dg-options "-fconstant-string-class=MyString" } */ 
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=MyString" { target *-*-darwin* } } */

#include <stdlib.h>

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
