// https://issues.dlang.org/show_bug.cgi?id=1170
type x;
mixin("alias int type;");

// https://issues.dlang.org/show_bug.cgi?id=10739
template DECLARE_HANDLE() {
    struct HINTERNET { int h; }
}
const INTERNET_INVALID_STATUS_CALLBACK = cast(INTERNET_STATUS_CALLBACK) -1;
mixin DECLARE_HANDLE;
alias void function(HINTERNET) INTERNET_STATUS_CALLBACK;
