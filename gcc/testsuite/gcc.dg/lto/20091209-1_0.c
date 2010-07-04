/* Stream an indirect edge in and out.  */

/* { dg-lto-do link } */
/* { dg-lto-options {{ -O3 -fno-early-inlining -flto }} } */

volatile int something;

static void hooray ()
{
  something = 1;
}

static void hiphip (void (*f)())
{
  something = 2;
  f ();
}

int main (int argc, int *argv[])
{
  hiphip (hooray);
  return 0;
}
