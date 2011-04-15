/* Testing for detecting duplicate ivars. */
/* { dg-do compile } */

typedef struct S { int i; } NSDictionary;

@interface A 
{
    NSDictionary * _userInfo; /* { dg-message "previous declaration" } */
    int i1;
    int i2;
    int i3;
    int i4;
    int i5;
    int i6;
    int i7;
    NSDictionary * _userInfo1; /* { dg-message "previous declaration" } */
}
@end

@interface B : A
{
    NSDictionary * _userInfo1; /* { dg-error "duplicate instance variable" } */
    int ii1;
    int ii2;
    int ii3;
    int ii4;
    int ii5;
    int ii6;
    int ii7;
}	
@end

@interface C : A
@end

@interface D : C
{
    NSDictionary * _userInfo;   /* { dg-error "duplicate instance variable" } */
}
@end

