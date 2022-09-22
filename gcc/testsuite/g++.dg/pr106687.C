// { dg-do run }
// { dg-options "-O2"  }

bool var_0 = (bool)0;
unsigned int var_7 = 42;
char var_215;

int main() {
    unsigned b = var_0;
    unsigned p2 = var_7;
    unsigned *tp;
    if (b < p2)
      tp = &p2;
    else
      tp = &b;
    unsigned tt = *tp;
    unsigned t = tt ^ (var_7 - var_0);
    var_215 = t ? t : 42;
    if (var_215 != 42)
      __builtin_abort();
    return 0;
}
