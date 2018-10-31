// REQUIRED_ARGS: -noboundscheck
// <-- To remove necessity of _D7imports13std11069array7__arrayZ

class Bar
{
    import imports.std11069container;

    BinaryHeap!(Foo[]) Heap;

    struct Foo {}
}

void main() {}
