// https://issues.dlang.org/show_bug.cgi?id=23089
extern(System) int i23089;

extern(System):

alias F23089 = void function(int);
F23089 f23089;
