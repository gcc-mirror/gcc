/* Make sure that bitfield types are printed correctly, and that ivar redeclaration
  (@interface vs. @implementation) checks take the bitfield width into account.  */
/* Author: Ziemowit Laski   <zlaski@apple.com>  */
/* { dg-do compile } */

@interface Base {
  int i;
}
@end

@interface WithBitfields: Base {
  void *isa;
  unsigned a: 3;
  signed b: 4;
  int c: 5;
}
@end

@implementation WithBitfields {
  char *isa;  /* { dg-error "conflicting instance variable type .char \\*isa." } */
  /* { dg-error "previous declaration of .void \\*isa." "" { target *-*-* } 12 } */
  unsigned a: 5;  /* { dg-error "conflicting instance variable type .unsigned( int)? a: 5." } */
  /* { dg-error "previous declaration of .unsigned( int)? a: 3." "" { target *-*-* } 13 } */                     
  signed b: 4;  /* This one is fine. */
  int c: 3;  /* { dg-error "conflicting instance variable type .int c: 3." } */
  /* { dg-error "previous declaration of .int c: 5." "" { target *-*-* } 15 } */ 
}
@end
