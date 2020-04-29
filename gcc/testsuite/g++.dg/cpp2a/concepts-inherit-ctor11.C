// PR c++/94819
// { dg-do compile { target concepts } }

struct dna4 {};
struct rna4 {};

template <typename component_types>
struct alphabet_tuple_base {
    template <typename component_type>
        requires __is_same(component_type, component_types)
    alphabet_tuple_base(component_type) {}
};

template <typename sequence_alphabet_t>
struct structured_rna : alphabet_tuple_base<sequence_alphabet_t> {
    using base_type = alphabet_tuple_base<sequence_alphabet_t>;
    using base_type::base_type;
};

structured_rna<rna4> t2{dna4{}}; // { dg-error "no match" }
structured_rna<rna4> t3{rna4{}}; // { dg-bogus "no match" }
