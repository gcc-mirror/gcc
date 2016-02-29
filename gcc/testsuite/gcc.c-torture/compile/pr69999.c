int uh;

void
ha(void)
{
  while (uh) {
    for (uh = 0; uh < 1; ++uh) {
      uh = 0;
      if (uh != 0)
 ts:
        uh %= uh;
    }
    ++uh;
  }
  goto ts;
}
