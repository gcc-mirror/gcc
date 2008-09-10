static void print_wkb_byte(unsigned char val) {
    print_wkb_bytes((unsigned char *)&val, 1, 1);
}
void InsertMultiPoint(int b) {
    char a = 1;
    if (b) a = 0;
    print_wkb_byte(a);
}

