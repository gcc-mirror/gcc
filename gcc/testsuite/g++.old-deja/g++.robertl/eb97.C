// Creates bad assembly on sparc and x86
template<unsigned long SIZE>
struct Array { };

template<unsigned long SIZE>
Array<SIZE> test_ok(const Array<SIZE>& a) {
    Array<SIZE> result;
    return(result);
}

template<unsigned long SIZE>
Array<SIZE + 1> test_error(const Array<SIZE>& a) {
    Array<SIZE + 1> result;
    return(result);
}

int main(int argc, char* argv[]) {
    Array<2> a;

    test_ok(a);
    test_error(a); // <<< MARKED LINE!

    return(0);
}
