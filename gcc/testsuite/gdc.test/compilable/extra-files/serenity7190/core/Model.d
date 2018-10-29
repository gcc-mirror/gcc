class SqlitePersister(T) {
    static assert(T.tupleof.length > 0, T.stringof ~ `(` ~ (T.tupleof.length + '0') ~ `): ` ~ T.tupleof.stringof);
}

class Model {}