// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }
// { dg-lto-options { { -O3 -fPIC -flto -shared } } }
// { dg-extra-ld-options "-Wl,-undefined,dynamic_lookup" { target *-*-darwin* } }

void PreEvaluate(void);
int main() { PreEvaluate(); return 0; }
