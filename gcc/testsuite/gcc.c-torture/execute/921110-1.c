extern int abort();
typedef int (*frob)();
frob f[] = {abort};
main()
{
  exit(0);
}
