// PR c++/94819
// { dg-do compile { target concepts } }

struct dna4 {};
struct rna4 {};

struct alphabet_tuple_base {
    template <typename component_type>
        requires __is_same(component_type, rna4)
    alphabet_tuple_base(component_type) {}
};

struct structured_rna : alphabet_tuple_base {
    using alphabet_tuple_base::alphabet_tuple_base;
};

structured_rna t2{dna4{}}; // { dg-error "no match" }
structured_rna t3{rna4{}}; // { dg-bogus "no match" }
