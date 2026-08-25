/* { dg-do run } */

/* CS-ELIM was miscompiling this
   by moving the store of v1.t across
   the load of v1 in `v2 = v1`.  */

/* This was reduced from isl manually.  */

struct s1 {
    int t;
};

struct s1 v1, v2;

[[gnu::noipa]]
void f(int a) {
    if (a) {
        v1.t = 1;
        v2 = v1;
    } else {
        v1.t = 2;
        v2 = v1;
    }
}

int main()
{
  f(1);
  if (v2.t != 1)
    __builtin_abort ();
  f(0);
  if (v2.t != 2)
    __builtin_abort ();
}
