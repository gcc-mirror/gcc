struct A { };
union U : A { };  // { dg-error "derived union 'U' invalid" }
U u;
