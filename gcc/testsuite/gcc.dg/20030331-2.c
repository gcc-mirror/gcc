// { dg-options "" }

extern int printf (const char *, ...);

int foo() {
  int yd;
  float in[1][yd];
 
  static void bar() {
    printf("%p\n",in[0]);
  }
}
