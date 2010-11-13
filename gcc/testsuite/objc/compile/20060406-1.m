/* This test tests typedefs and protocol qualifiers.  */

@protocol O
- (unsigned)j;
@end

@interface T
@end


/* First test.  */
typedef T<O> *S;

@interface I
+ (unsigned char)T:(S[2])p
                 v:(S)h;
@end

@implementation I
+ (unsigned char)T:(S[2])p
                 v:(S)h
{
  p[0] = (S) 0;
  p[1] = (S) 0;
  return 0;
}
@end


/* Second test.  */
typedef T<O> S1;

@interface I1
+ (unsigned char)T1:(S1*[2])p 
                 v1:(S1*)h;
@end

@implementation I1
+ (unsigned char)T1:(S1*[2])p
                 v1:(S1*)h
{
  p[0] = (S1*) 0;
  p[1] = (S1*) 0;
  return 0;
}
@end


/* Third test.  */
typedef T S2;

@interface I2
+ (unsigned char)T1:(S2<O>*[2])p 
                 v1:(S2<O>*)h;
@end

@implementation I2
+ (unsigned char)T1:(S2<O>*[2])p
                 v1:(S2<O>*)h
{
  p[0] = (S2<O>*) 0;
  p[1] = (S2<O>*) 0;
  return 0;
}
@end
