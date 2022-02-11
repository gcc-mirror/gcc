enum E:char[4]{ str = "abcd" }
enum x = {
    int[char[4]] aa;
    aa[E.str] = 1;
    foreach(key,val; aa) {}
    return aa["abcd"];
}();
