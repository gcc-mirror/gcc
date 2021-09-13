module trait_loc_ov_err;
/*
TEST_OUTPUT:
---
fail_compilation/trait_loc_ov_err.d(24): Error: cannot get location of an overload set, use `__traits(getOverloads, ..., "ov1")[N]` to get the Nth overload
fail_compilation/trait_loc_ov_err.d(25): Error: cannot get location of an overload set, use `__traits(getOverloads, ..., "ov2")[N]` to get the Nth overload
---
*/

void ov1(){}
void ov1(int){}

void ov21(){}
void ov22(int){}
alias ov2 = ov21;
alias ov2 = ov22;

template OvT(T, U){}
template OvT(T){}

auto func(T)(T t) {}
auto func(T,U)(T t,U u) {}

enum e1 = __traits(getLocation, ov1);
enum e2 = __traits(getLocation, ov2);

enum e3 = __traits(getLocation, OvT);
enum e4 = __traits(getLocation, func);

enum e5 = __traits(getLocation, __traits(getOverloads, trait_loc_ov_err, "ov1")[0]);
enum e6 = __traits(getLocation, __traits(getOverloads, trait_loc_ov_err, "ov1")[1]);

enum e7 = __traits(getLocation, __traits(getOverloads, trait_loc_ov_err, "ov2")[0]);
enum e8 = __traits(getLocation, __traits(getOverloads, trait_loc_ov_err, "ov2")[1]);

enum e9  = __traits(getLocation, __traits(getOverloads, trait_loc_ov_err, "OvT", true)[1]);
enum e10 = __traits(getLocation, __traits(getOverloads, trait_loc_ov_err, "OvT", true)[0]);

enum e11 = __traits(getLocation, __traits(getOverloads, trait_loc_ov_err, "func", true)[0]);
enum e12 = __traits(getLocation, __traits(getOverloads, trait_loc_ov_err, "func", true)[1]);
