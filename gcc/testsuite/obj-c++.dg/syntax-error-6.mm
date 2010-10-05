/* { dg-do compile } */

@interface NSButton
- (int) state;
@end

void FOO()
{
  NSButton * mCopyAcrobatCB; 
	
  [ [ mCopyAcrobatCB state ] == 0 ] != 1;  /* { dg-error "objective\\-c\\+\\+" } */
}
