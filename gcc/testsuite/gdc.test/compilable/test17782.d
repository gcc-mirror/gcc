void main() {
    string str = q"_DLANG
123
_DLANG";
    assert( str == "123\n" );
}
