// { dg-do compile { target c++14 } }
// { dg-options "-fconcepts" }

template <class T>
concept bool Destructible() {
    return false;
}

template <class T, class...Args>
concept bool ConstructibleObject =
    // Concept evaluation should short-circuit even the template
    // substitution, so we shouldn't even substitute into the requires
    // constraint and the unimplemented multi-dimensional new T{...}
    // initialization.  ATM we do, but as long as we don't output the
    // sorry() message we used to for such constructs when asked not
    // to issue errors, this shouldn't be a problem for this and
    // similar cases.
    Destructible<T>() && requires (Args&&...args) {
        new T{ (Args&&)args... };
    };

int main() {
    using T = int[2][2];
    // GCC has not implemented initialization of multi-dimensional
    // arrays with new{} expressions.
    static_assert(!ConstructibleObject<T, T>);
}
