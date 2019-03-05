// { dg-lto-do assemble }

struct Main { Main(char* x); };

Main::Main(char* x) {
    char cfg[__builtin_strlen(x)];
}
