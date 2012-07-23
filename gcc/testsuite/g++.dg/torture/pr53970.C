// { dg-do run }

#pragma pack(1)
struct mystruct {
    char c;
    unsigned long l[1024];
};
#pragma pack()

int main(int argc, char **argv)
{
  mystruct *a = new mystruct;
  unsigned long i;
  for (i = 0; i < 1024; ++i)
    a->l[i] = 0xdeadbeaf;
  return 0;
}
