/* { dg-do compile } */

@interface NSButton
- (int) state;
@end

void FOO()
{
  NSButton * mCopyAcrobatCB; 
	
  [ [ mCopyAcrobatCB state ] == 0 ] != 1;  /* { dg-error "expected identifier before ... token" } */
/* { dg-error "expected \\\'\\\{\\\' before \\\'!=\\\' token" "" { target *-*-* } 11 } */
/* { dg-error "lambda expressions only available with" "" { target *-*-* } 11 } */
/* { dg-error "no match for \\\'operator!=\\\' in" "" { target *-*-* } 11 } */
}
