// REQUIRED_ARGS: -profile=gc
struct T(string s) {}
alias TypeWithQuotes = T!q"EOS
`"'}])>
EOS";

void foo() {
    TypeWithQuotes[] arr;
    arr ~= TypeWithQuotes();
}
