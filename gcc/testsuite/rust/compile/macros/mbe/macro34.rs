macro_rules! allowed_after_expr_matcher {
    (($t:expr) bok) => {{}}; // follow-set restrictions do not apply after a matcher, but they do apply inside the matcher
}
