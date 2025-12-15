static int bug(const char *dirname) {
    dirname++;
    return __builtin_strlen(dirname); /* { dg-bogus "buffer over-read" } */
}

int main (void) {
    return bug("abcd");
}
