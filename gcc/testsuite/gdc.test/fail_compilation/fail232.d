void bug1601() {
    int i;

    i = i >> 33;
    i = i << 33;
    i = i >>> 33;
}
