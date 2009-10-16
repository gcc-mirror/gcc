typedef union tree_node *tree;
union tree_node {
    tree * use;
    VEC_constructor_elt_gc *elts;
};

