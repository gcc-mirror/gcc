// { dg-error "'loop' is not allowed in const context" "" { target *-*-* } .+1 }
const CONST_LOOP : () = loop{};

// { dg-error "'loop' is not allowed in const context" "" { target *-*-* } .+1 }
static STATIC_LOOP : () = loop{};