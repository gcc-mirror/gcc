// PERMUTE_ARGS:
// https://issues.dlang.org/show_bug.cgi?id=15780

import std.typecons;
//import std.stdio;

void foo(alias fields)() {
    foreach(i, field; fields) {
        enum string a = fields[i];  // OK
        enum string b = field;      // not OK with 2.069.2 ???
	//writeln(field);
    }
}

void main() {
    foo!(tuple("H", "I"))();
}
