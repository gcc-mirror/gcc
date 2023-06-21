/* { dg-do run } */
/* { dg-options "-w -fpermissive" } */
/* { dg-shouldfail "asan" } */

int a;
int *b = &a;
int **c = &b;
int d[1];
int *e = &d[1];

static void f(int *g) {
  *b = e;
  *c = e;
  *b = 2;
  *g = 2;
}

int main() {
    f(b);
    return *b;
}

/* { dg-output "AddressSanitizer: global-buffer-overflow on address" } */
