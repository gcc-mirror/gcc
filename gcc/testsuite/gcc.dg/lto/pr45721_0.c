/* { dg-lto-do assemble }  */
void baz(void) {}
void *y = (void *)baz;
int main () { return 0; }
