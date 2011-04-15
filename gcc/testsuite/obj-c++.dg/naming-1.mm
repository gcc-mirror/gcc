/* Testing for detecting duplicate ivars. */
/* { dg-do compile } */

typedef struct S { int i; } NSDictionary;

@interface A 
{
    NSDictionary * _userInfo1; /* { dg-message "previous declaration" } */
    NSDictionary * _userInfo2; /* { dg-message "previous declaration" } */
    NSDictionary * _userInfo3; /* { dg-message "previous declaration" } */
    NSDictionary * _userInfo4; /* { dg-message "previous declaration" } */
}
@end

@interface B : A
{
    NSDictionary * _userInfo1;	/* { dg-error "duplicate instance variable" } */
    NSDictionary * _userInfo2;	/* { dg-error "duplicate instance variable" } */
}
@end

@interface C : A
@end

@interface D : C
{
    NSDictionary * _userInfo3;  /* { dg-error "duplicate instance variable" } */
    NSDictionary * _userInfo4;  /* { dg-error "duplicate instance variable" } */
}
@end

