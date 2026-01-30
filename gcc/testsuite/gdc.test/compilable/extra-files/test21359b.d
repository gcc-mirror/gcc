struct SumType(Types...)
{
    static foreach (T; Types)
        T values_;
}

SumType!(string[int]) convertObserver;

void hackAroundLinkerError() {
    auto t = typeid(const(immutable(char)[][]));
}
