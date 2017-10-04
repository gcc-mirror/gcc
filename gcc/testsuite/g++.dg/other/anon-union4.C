// PR c++/64644

static union { };  // { dg-error "anonymous union with no members" }
