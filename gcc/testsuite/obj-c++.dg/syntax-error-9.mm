@implementation SaturnDoc /* { dg-warning "cannot find interface declaration" } */
- read: (void*)aStream ggg /* { dg-error "expected .:. at end of input" } */
/* { dg-error "-:expected ..end. at end of input" "" { target *-*-* } .+1 } */
