/* Only needs to compile [see PR 49070 for C++ issue].  */

#if __has_attribute(__objc_root_class__)
__attribute__((__objc_root_class__))
#endif
@interface A
- (id) :(id)arg0 :(id)arg1;
- (id) m:(id)arg0 :(id)arg1 :(id)arg2 :(id)arg3;
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
@end

id f1 (A *x)
{
  return [x:x:x];
}

id f2 (A *x)
{
  return [x m:x:x:x:x];
}
