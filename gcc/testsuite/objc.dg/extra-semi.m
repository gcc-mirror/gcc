/* Allow extra semicolons in between method declarations,
   for old times' sake.  */

/* { dg-do compile } */

@interface Foo
   -(Foo *) expiration;
   -(void) setExpiration:(Foo *) date;;
   -(int) getVersion;
@end
