/* { dg-do compile } */
/* Contributed by Alexander Malmberg: PR18456 */

@interface Foo
-(void) foo;
@end

void *ip;

void (*func1)(void);

struct
{
    int a:2;
} struct1,struct2[2];

union
{
    int a:2;
} union1,union2[2];

Foo **f;

int main(int argc,char **argv)
{
  [(struct {int a;} *)ip foo];  /* { dg-warning "invalid receiver type" } */
  [func1 foo];                  /* { dg-warning "invalid receiver type" } */
  [struct1.a foo];              /* { dg-warning "invalid receiver type" } */
                                /* { dg-warning "cast to pointer from integer" "" { target *-*-* } .-1 } */ 
  [union1.a foo];               /* { dg-warning "invalid receiver type" } */
                                /* { dg-warning "cast to pointer from integer" "" { target *-*-* } .-1 } */ 
  [struct1 foo];                /* { dg-warning "invalid receiver type" } */
                                /* { dg-error "cannot convert" "" { target *-*-* } .-1 } */ 
  [union1 foo];                 /* { dg-warning "invalid receiver type" } */
                                /* { dg-error "cannot convert" "" { target *-*-* } .-1 } */ 
  [struct2 foo];                /* { dg-warning "invalid receiver type" } */
                                /* { dg-error "cannot convert" "" { target *-*-* } .-1 } */ 
  [union2 foo];                 /* { dg-warning "invalid receiver type" } */
                                /* { dg-error "cannot convert" "" { target *-*-* } .-1 } */ 
  [f foo];                      /* { dg-warning "invalid receiver type" } */
}
