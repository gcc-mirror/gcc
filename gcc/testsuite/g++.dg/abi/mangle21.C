// PR c++/14324
// { dg-do assemble }

extern "C" {

void fun1(void)
{
  do { static int xyz __attribute__((unused)) = 1; } while (0);
  do { static int xyz __attribute__((unused)) = 2; } while (0);
  do { static int xyz __attribute__((unused)) = 3; } while (0);
}

}
