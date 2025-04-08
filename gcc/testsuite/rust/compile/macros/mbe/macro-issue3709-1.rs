macro_rules! doc_comment {
    (#[ $attr: meta ]) => {
        #[$attr]
        struct Generated; // { dg-warning "never constructed" }
    };
}

doc_comment! {
    /// This is a generated struct
}
