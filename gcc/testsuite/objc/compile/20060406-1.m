typedef struct
{
  void *p;
} *S;

@protocol O
- (unsigned)j;
@end

@interface I
+ (unsigned char)T:(S<O>[2])p v:(S<O>)h;
@end

@implementation I
+ (unsigned char)T:(S<O>[2])p v:(S<O>)h
{
  p[0] = (S) 0;
  p[1] = (S) 0;
  return 0;
}
@end
