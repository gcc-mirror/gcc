template types (T) {
    static if (is (T V : V[K], K == class)) {
        static assert (false, "assoc");
    }
    static const int types = 4;
}

int i = types!(int);
