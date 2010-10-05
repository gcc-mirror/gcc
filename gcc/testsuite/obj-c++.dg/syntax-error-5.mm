/* { dg-do compile } */

typedef struct S { int i; } NSDictionary;

@interface A 
{
}
@end

@interface B : A
{
    NSDictionary * _userInfo;
@end				/* { dg-error "expected .\}. before .end."  } */
