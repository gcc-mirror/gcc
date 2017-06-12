/* Reject ivars with an unknown size.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

struct unknownStruct;

@interface ArrayTest
{
    short   unknownSize[unknownValue];  /* { dg-error ".unknownValue. (undeclared|was not declared)" } */
    /* { dg-error "instance variable .unknownSize. has unknown size" "" { target *-*-* } .-1 } */
    struct unknownStruct unknownObj;  /* { dg-error "field .unknownObj. has incomplete type" } */
    /* { dg-error "instance variable .unknownObj. has unknown size" "" { target *-*-* } .-1 } */
    long    knownSize[3];     /* ok */
    char    zeroSize[2 - 2];  /* ok (apparently) */
    int     missingSize[];  /* { dg-error "instance variable .missingSize. has unknown size" } */
}
@end
