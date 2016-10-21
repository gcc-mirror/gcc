extern "C" {
    typedef int FILE;
    int *stdout;
    int fputs(const char *, FILE *);
}
void print_packet() {
    const char *color[]{"", "", ""};
    fputs(color[2], stdout);
}
