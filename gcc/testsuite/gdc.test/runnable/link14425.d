struct SFoo(I) { I i; }
struct SBar(I) { I i; }
static assert(is(SFoo!(SBar!string)));

class CFoo(I) { I i; }
class CBar(I) { I i; }
static assert(is(CFoo!(CBar!string)));

void main() {}
