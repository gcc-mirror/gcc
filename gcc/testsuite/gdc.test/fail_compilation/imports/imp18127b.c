// https://github.com/dlang/dmd/issues/18127
// struct in here, union in other
struct struct_or_union {
    int x;
};

// mismatching number of fields
struct S_n_fields {
    int x;
};

// mismatched types
struct S_types {
    int x;
};

// mismatched names
struct S_names {
    float y;
};

struct B {
    float x;
};

// Contains a struct that is incompatible
struct S_b {
    struct B b;
};

// mismatched anonymous struct
struct S_contains_anon_named {
    struct {
        float x;
    } a;
};

struct S_contains_anon_unnamed {
    struct {
        float x;
    };
};

// bitfields
struct S_bitfields_mismatch1 {
    unsigned x: 3;
    unsigned y;
};
struct S_bitfields_mismatch2 {
    unsigned x: 3;
    unsigned y: 1;
};

struct S_bitfields_widths {
    unsigned x: 3;
    unsigned y: 2;
};

struct S_bitfields_anon {
    unsigned x: 3;
    unsigned y: 1;
};

// mismatched alignment
struct S_alignas {
    float x;
};
struct S_aligned {
    float x;
}__attribute__((aligned(4)));

// mismatched packing
struct S_pack_1 {
    float x;
    char c;
};
struct S_pack_2 {
    float x;
    char c;
};
