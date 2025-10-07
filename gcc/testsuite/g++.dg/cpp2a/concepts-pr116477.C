// { dg-do compile { target c++20 } }

template <typename T>
concept my_concept = true;

template <typename G>
void run () {
    my_concept<G> (G{});  // { dg-error "cannot call a concept as a function" }
}
