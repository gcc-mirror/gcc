// { dg-additional-options "-fmodules-ts" }
// reduced from string-view1 through cvise. Broken under c++2a too.
// we were creating a hash slot, and then doing another lookup without
// initializing that first slot :(

namespace std {
typedef int a;
int b;
decltype(nullptr) c;
namespace xyz {}
__builtin_va_list d;
int n;
int e;
int f;
int g;
int h;
int i;
int j;
int k;
typedef struct l m;
typedef struct aa w;
typedef struct x o;
typedef x p;
long q;
long r;
typedef l s;
extern p ab;
void t();
void v();
extern p ac;
void ad();
int ae;
int af;
extern p ag;
extern p ah;
void ai();
void y();
int aj;
int ak;
int al;
char am;
int an;
a ao;
int ap;
int aq;
void z();
int ar;
int as;
void at();
void au();
void av();
void aw();
int u;
namespace zz {
}
}
