// PR c++/113405

template <typename>
concept foo = false;

template <typename>
concept bar = true;

template <typename T>
struct corge {};

template <foo F>
struct corge<F> {};

template <bar B>
struct corge<B> {
  using alias = int;
};
