/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

char onelock_lock[16];
void write(void);

void lockit(int count) {
  for (; count;) {
    int pid, i;
    char *p;
    for (i = 0, p = (char *)&pid; i < sizeof 0; i++)
      onelock_lock[i] = *p++;
    write();
  }
}
