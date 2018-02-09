void* a;

void foo() {
    if ((a = &&l))
        return;

    l:;
}

int main() {
    foo();
    goto *a;

    return 0;
}
