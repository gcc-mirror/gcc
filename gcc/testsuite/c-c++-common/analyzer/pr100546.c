#include <stdio.h>
#include <stdlib.h>

static void noReturn(const char *str) __attribute__((noreturn));
static void noReturn(const char *str) {
    printf("%s\n", str);
    exit(1);
}

void (*noReturnPtr)(const char *str) = &noReturn;

int main(int argc, char **argv) {
    char *str = 0;
    if (!str)
        noReturnPtr(__FILE__);
    return printf("%c\n", *str);
}
