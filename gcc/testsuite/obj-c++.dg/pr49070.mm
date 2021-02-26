/* Only needs to compile.  */
/* { dg-additional-options "-std=c++11" } */

#ifdef __cplusplus
enum X {
  x = 5,
  y
};
#endif

#if __has_attribute(__objc_root_class__)
__attribute__((__objc_root_class__))
#endif
@interface A
- (id) :(id)arg0 :(id)arg1;
- (id) m:(id)arg0 :(id)arg1 :(id)arg2 :(id)arg3;
#ifdef __cplusplus
- (id) n:(X)arg0 :(X)arg1 :(id)arg2 :(id)arg3;
#endif
@end

@implementation A
- (id) :(id)arg0 :(id)arg1
{
  return arg1;
}
- (id) m:(id)arg0 :(id)arg1 :(id)arg2 :(id)arg3
{
  return arg2;
}
#ifdef __cplusplus
- (id) n:(X)arg0 :(X)arg1 :(id)arg2 :(id)arg3
{
  return arg2;
}
#endif
@end

id f1 (A *x)
{
  return [x:x:x];
}

id f2 (A *x)
{
  return [x m:x:x:x:x];
}

#ifdef __cplusplus
id f3 (A *x)
{
  return [x n:X::x:X::y:x:x];
}
#endif
