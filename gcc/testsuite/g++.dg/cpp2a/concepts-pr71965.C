// { dg-do compile { target c++20 } }

template <class T>
concept Destructible = false;

template <class T, class...Args>
concept ConstructibleObject =
    // Concept evaluation should short-circuit even the template
    // substitution, so we shouldn't even substitute into the requires
    // constraint and the unimplemented multi-dimensional new T{...}
    // initialization.  ATM we do, but as long as we don't output the
    // sorry() message we used to for such constructs when asked not
    // to issue errors, this shouldn't be a problem for this and
    // similar cases.
    Destructible<T> && requires (Args&&...args) {
        new T{ (Args&&)args... };
    };

int main() {
    using T = int[2][2];
    // GCC has not implemented initialization of multi-dimensional
    // arrays with new{} expressions.
    static_assert(!ConstructibleObject<T, T>);
}
