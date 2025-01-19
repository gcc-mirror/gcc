// EXTRA_FILES: imports/std11069array.d imports/std11069container.d imports/std11069range.d imports/std11069typecons.d
// REQUIRED_ARGS: -boundscheck=off
// <-- To remove necessity of _D7imports13std11069array7__arrayZ

class Bar
{
    import imports.std11069container;

    BinaryHeap!(Foo[]) Heap;

    struct Foo {}
}

void main() {}
