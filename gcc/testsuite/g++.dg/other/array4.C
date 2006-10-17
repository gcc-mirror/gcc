// PR C++/28906: new on an array causes incomplete arrays to
// become complete with the wrong size.
// The sizeof machineMain should be 5 and not 100.
// { dg-do run }


extern char machineMain[];
void sort (long len)
{
  new char[100];
}
char machineMain[] = "main";
int main(void)
{
  if (sizeof(machineMain)!=sizeof("main"))
    __builtin_abort();
}


