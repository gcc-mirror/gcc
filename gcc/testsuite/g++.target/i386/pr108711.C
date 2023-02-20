/* PR target/108711.C */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-exceptions" } */
struct Expression_list {
  Expression_list *copy();
} vals_;
struct Parser_expression {
  Parser_expression();
};
struct Composite_literal_expression : Parser_expression {
  Composite_literal_expression(bool has_keys, Expression_list *,
                               bool all_are_names)
      : has_keys_(has_keys), all_are_names_(all_are_names) {}
  void do_copy();
  bool has_keys_;
  bool all_are_names_;
};
void Composite_literal_expression::do_copy() {
  new Composite_literal_expression(has_keys_, vals_.copy(), all_are_names_);
}
