// https://issues.dlang.org/show_bug.cgi?id=15711

struct Quu {
    string val;
}

string[] result = foo!(0, [Quu(['z']), Quu("")]);

template foo(size_t i, Quu[] data, string[] results = []) {
    static if (i < data.length) {
        enum def = data[i];
        enum foo = foo!(i+1, data, results ~ def.val);
    }
    else {
        enum foo = results;
    }
}

// Run-time version already works

string[] result_rt = foo_rt(0, [Quu(['z']), Quu("")]);

string[] foo_rt(size_t i, Quu[] data, string[] results = []) {
    if (i < data.length) {
        auto def = data[i];
        return foo_rt(i+1, data, results ~ def.val);
    }
    else {
        return results;
    }
}
