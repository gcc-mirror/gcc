// { dg-do compile }
int a;
void b(bool c[], char d[], bool g[][55][21]) {
  for (signed e = 0; e < 11; e += 3)
    for (unsigned f = c[0] + 1; f < d[0]; f += 3)
      a = g[0][e][f + 2];
}

