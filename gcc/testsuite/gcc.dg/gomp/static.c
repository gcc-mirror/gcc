static int bork;

void bar(void);

void foobar (void)
{
#pragma omp parallel
  {
#pragma omp for lastprivate(bork)
    for (bork = 0; bork < 100; bork++) {
        bar();
    }
  }
}
