#include <stdlib.h>
struct point { double x, y, z; };

@interface Foo
- (struct point)bar;
- (struct point)baz;
@end

@implementation Foo
- (struct point)bar { struct point q = { 1.0, 2.0, 3.0 }; return q; };
- (struct point)baz { struct point q = { 4.0, 5.0, 6.0 }; return q; };
@end

/* Cases where a check for nil should be inserted by the compiler, when
   -fobjc-nilcheck is in force. We should not attempt the calls, and the
   result should be 0-filled. */

Foo *f;

int main(void) {
  struct point p = [f bar];
  if (p.x != 0.0 || p.y != 0.0 || p.z != 0.0)
    abort ();
  id nilobj = (id)0;
  p = [nilobj baz];
  if (p.x != 0.0 || p.y != 0.0 || p.z != 0.0)
    abort ();
  return 0;
}
