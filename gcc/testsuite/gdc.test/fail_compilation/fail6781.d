void bug6781(alias xxx)() {
    some_error;
}
struct C6781 {
    void makeSortedIndices() {
        int greater;
        bug6781!greater();
    }
}
