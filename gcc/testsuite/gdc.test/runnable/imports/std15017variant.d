module imports.std15017variant;

struct VariantN(uint maxDataSize)
{
    VariantN opAssign() { return this; }

    bool opEquals(T)(T) { return true; }

    bool opEquals(T)(ref const T) const { return true; }

    size_t toHash() const nothrow @trusted { return 0;  }

    ~this() {}
}

alias Variant1 = VariantN!(1);
alias Variant2 = VariantN!(2);
alias Variant3 = VariantN!(3);
alias Variant4 = VariantN!(4);
alias Variant5 = VariantN!(5);
alias Variant6 = VariantN!(6);
alias Variant7 = VariantN!(7);
alias Variant8 = VariantN!(8);
alias Variant9 = VariantN!(9);
alias Variant10 = VariantN!(10);
