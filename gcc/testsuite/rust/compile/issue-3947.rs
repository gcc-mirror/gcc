enum _Enum {
    A(),
}

type _E = _Enum;

// { dg-warning "function is never used: '_a'" "" { target *-*-* } .+1 }
const fn _a() -> _Enum {
    _E::A()
}
