// https://github.com/dlang/dmd/issues/18127
// union in here, struct in other
union struct_or_union {
    int x;
};

// mismatching number of fields
struct S_n_fields {
    int x, y;
};

// mismatched types
struct S_types {
    float x;
};

// mismatched names
struct S_names {
    float x;
};

struct B {
    int x;
};

// Contains a struct that is incompatible
struct S_b {
    struct B b;
};

// mismatched anonymous struct
struct S_contains_anon_named {
    struct {
        int x;
    } a;
};

struct S_contains_anon_unnamed {
    struct {
        int x;
    };
};

// bitfields
struct S_bitfields_mismatch1 {
    unsigned x: 3;
    unsigned y: 1;
};
struct S_bitfields_mismatch2 {
    unsigned x;
    unsigned y: 1;
};

struct S_bitfields_widths {
    unsigned x: 3;
    unsigned y: 1;
};

struct S_bitfields_anon {
    unsigned x: 3;
    unsigned : 1;
};

// mismatched alignment
struct S_alignas {
    _Alignas(8) float x;
};
struct S_aligned {
    float x;
}__attribute__((aligned(8)));

// mismatched packing
struct __attribute__((packed)) S_pack_1 {
    float x;
    char c;
};
#pragma pack(push)
#pragma pack(1)
struct S_pack_2 {
    float x;
    char c;
};
#pragma pack(pop)
