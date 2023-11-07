// { dg-skip-if "c++98 does not support attributes" { c++98_only } }

[[omp::decl (declare target, indirect(1))]] // { dg-error "directive with only 'device_type' or 'indirect' clause" }
int f (void) { return 5; }

[[omp::decl (declare target indirect)]] // { dg-error "directive with only 'device_type' or 'indirect' clause" }
int g (void) { return 8; }

[[omp::directive (begin declare target, indirect)]];
int h (void) { return 11; }
[[omp::directive (end declare target)]];

int i (void) { return 8; }
[[omp::directive (declare target to(i), indirect (1))]];

int j (void) { return 11; }
[[omp::directive (declare target indirect enter (j))]];
