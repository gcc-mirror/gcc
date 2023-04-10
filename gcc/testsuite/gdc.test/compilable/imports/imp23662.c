// https://issues.dlang.org/show_bug.cgi?id=23662
typedef enum {A} E;

E func(E v) {
    return v;
}
