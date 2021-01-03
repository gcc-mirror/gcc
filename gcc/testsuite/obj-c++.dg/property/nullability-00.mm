/* { dg-do compile } */
/* { dg-additional-options "-fsyntax-only" } */

@interface MyRoot
{
  Class isa __attribute__((deprecated));
  id p;
  int x;
  int *i;
}

@property(null_unspecified, assign) MyRoot *p1;
@property(nonnull, assign) MyRoot *p2;
@property(nullable, assign) MyRoot *p3;
@property(null_resettable, assign) MyRoot *p4;
@property(null_exciting, assign) MyRoot *e_5; /* { dg-error {unknown property attribute 'null_exciting'} } */

@property(nonnull, retain, nullable) MyRoot *e_6; /* { dg-error {'nullable' attribute conflicts with 'nonnull' attribute} } */
@property(nonnull, nonnull) int *i; /* { dg-warning {duplicate 'nonnull' attribute} } */

@end
