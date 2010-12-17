/* { dg-do compile } */

@interface SomeClass
+ method:(int)foo;
@end

int main(void) {
    [SomeClass method:3, 4];	/* { dg-error "too many arguments to method \\'method:\\'" } */
    return 0;
}
