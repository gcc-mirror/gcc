// Build don't link:
// prms-id: 9506

char * volatile p;
void foo() {
  --p = 0;
}
