char temp[] = "192.168.190.160";
unsigned result = (((((192u<<8)|168u)<<8)|190u)<<8)|160u;

int strtoul1(const char *a, char **b, int c) __attribute__((noinline, noclone));
int strtoul1(const char *a, char **b, int c)
{
  *b = a+3;
  if (a == temp)
    return 192;
  else if (a == temp+4)
    return 168;
  else if (a == temp+8)
    return 190;
  else if (a == temp+12)
    return 160;
  __builtin_abort();
}

int string_to_ip(const char *s) __attribute__((noinline,noclone));
int string_to_ip(const char *s)
{
        int addr;
        char *e;
        int i;

        if (s == 0)
                return(0);

        for (addr=0, i=0; i<4; ++i) {
                int val = s ? strtoul1(s, &e, 10) : 0;
                addr <<= 8;
                addr |= (val & 0xFF);
                if (s) {
                        s = (*e) ? e+1 : e;
                }
        }

        return addr;
}

int main(void)
{
  int t = string_to_ip (temp);
  printf ("%x\n", t);
  printf ("%x\n", result);
  if (t != result)
    __builtin_abort ();
  printf ("WORKS.\n");
  return 0;
}

