// PR c++/114772
// { dg-do compile { target x86_64-*-* } }

template<typename V, bool STREAMING>
inline __attribute__((always_inline))
__attribute__((warn_unused_result))
int walk_document(V visitor) {return 0;}

template<bool STREAMING>
void parse_document() {
    int r = walk_document<bool, STREAMING>(false);
}

void stage2_next() {
    parse_document<true>();
}

#pragma GCC target("pclmul")
