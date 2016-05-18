// PR c++/69793

class fpos;
template < state > bool operator!= (fpos,; operator!= // { dg-error "declared|expected|type" }
