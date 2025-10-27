/* { dg-do run } */
/* { dg-options "-O1" } */

struct s4 {
    int _0;
};
struct s1 {
    unsigned char _4;
    long _1;
};
struct s2 {
    union {
        struct s3 {
            unsigned char _1;
            struct s4 _0;
        } var_0;
        struct s1 var_1;
    } DATA;
};
int f1(int arg0) { return arg0 > 12345; }
__attribute__((noinline))
struct s4 f2(int arg0) {
    struct s4 rv = {arg0};
    return rv;
}
struct s2 f3(int arg0) {
    struct s2 rv;
    struct s1 var6 = {0};
    struct s4 var7;
    if (f1(arg0)) {
        rv.DATA.var_1 = var6;
        return rv;
    } else {
        rv.DATA.var_0._1 = 2;
        var7 = f2(arg0);
        rv.DATA.var_0._0 = var7;
        return rv;
    }
}
int main() {
  if (f3(12345).DATA.var_0._0._0 == 12345)
    ;
  else
    __builtin_abort();
  if (f3(12344).DATA.var_0._0._0 == 12344)
    ;
  else
    __builtin_abort();
}
