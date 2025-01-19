// https://issues.dlang.org/show_bug.cgi?id=24760

long f(int e = 0, uint[] optional...) => optional.length;
long f0() => f(); // compiler segfaults
