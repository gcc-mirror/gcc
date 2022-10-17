module imports.test13a;

extern(C) int printf(const char*, ...);

public alias bool boolean;
public alias int Order;
public const Order LESS_THAN    = -1;
public const Order EQUALS_TO    = 0;
public const Order GREATER_THAN = +1;
template Ordinal(T) {
    public T min(T left, T right) {
        return left < right ? left: right;
    }
    public T max(T left, T right) {
        return left > right ? left: right;
    }
    public T clamp(T item, T lower, T upper)
    in {
        assert(lower <= upper);
    } do {
        return max(min(item, upper), lower);
    }
}
template TPair(T, U) {
    public class Pair {
        private T _left;
        private U _right;
        public this(T left, U right) {
            this._left = left;
            this._right = right;
        }
        public T left() {
            return this._left;
        }
        public U right() {
            return this._right;
        }
        override public boolean opEquals(Object obj) {
            Pair other = cast(Pair) obj;
            if (other !is null) {
                return (left() == other.left()) && (right() == other.right());
            } else {
                return false;
            }
        }
    }
}
unittest {
    alias TPair!(char, char) charPair;
    charPair.Pair pairA = new charPair.Pair('a', 'b');
    charPair.Pair pairB = new charPair.Pair('a', 'b');
    assert(pairA == pairB);
    printf("Pair tests passed!\r\n");
}
