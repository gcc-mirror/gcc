// REQUIRED_ARGS: -preview=dip1000

// https://issues.dlang.org/show_bug.cgi?id=18670

void foo() {
    new OVERLAPPED;
}

union OVERLAPPED {
    uint     OffsetHigh;
    uint     Pointer;
}
