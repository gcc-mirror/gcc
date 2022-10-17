enum X {
    O,
    R
}
enum Y {
    U
}
static assert( (X.O == cast(const)X.O));
static assert( (X.O == X.O));
static assert( (X.O != X.R));
