/* { dg-do compile } */
/* { dg-options "-O2" } */

void some_memcpy(void *, long);
long bufused;
char buf, otest_s;
void otest(...) {
  long slength;
  some_memcpy(&buf + bufused, slength & otest_s);
}
int f, finish_root_table_fli2_1;
static void finish_root_table(char *lastname) {
  for (;;)
    if (finish_root_table_fli2_1)
      otest(f, lastname);
}
void write_roots() { finish_root_table(""); }
