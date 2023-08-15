// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=110959
// { dg-do compile }
class ArsdExceptionBase : object.Exception {
    this(string operation, string file = __FILE__, size_t line = __LINE__, Throwable next = null) {
        super(operation, file, line, next);
    }
}

template ArsdException(alias Type, DataTuple...) {
    static if(DataTuple.length)
        alias Parent = ArsdException!(Type, DataTuple[0 .. $-1]);
    else
        alias Parent = ArsdExceptionBase;

    class ArsdException : Parent {
        DataTuple data;

        this(DataTuple data, string file = __FILE__, size_t line = __LINE__) {
            this.data = data;
            static if(is(Parent == ArsdExceptionBase))
                super(null, file, line);
            else
                super(data[0 .. $-1], file, line);
        }

        static opCall(R...)(R r, string file = __FILE__, size_t line = __LINE__) {
            return new ArsdException!(Type, DataTuple, R)(r, file, line);
        }
    }
}

__gshared pr110959 = ArsdException!"Test"(4, "four");
