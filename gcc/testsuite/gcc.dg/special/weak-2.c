/* { dg-do run } */

extern int foo(void);

int main(void) {

    if (foo())
        exit(0);
    else
        abort();
}
