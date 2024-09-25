/* { dg-do run } */
/* { dg-options "-O2 -std=c99" } */


int puts(const char *);
int snprintf(char *, __SIZE_TYPE__, const char *, ...);
__SIZE_TYPE__ strspn(const char *, const char *);

struct TValue {
  union {
    long long i;
    double n;
  } value_;
  unsigned char tt_;
};

static int tostringbuff (struct TValue *num, char *str) {
  int len;
  if (num->tt_ == 3) {
    len = snprintf(str,50,"%lld",num->value_.i);
  } else {
    len = snprintf(str,50,"%.14g",num->value_.n);
    if (str[strspn(str, "-0123456789")] == '\0') {
      str[len++] = '.';
      str[len++] = '0';
    }
  }
  return len;
}

void unused (int *buff, struct TValue *num) {
  char junk[50];
  *buff += tostringbuff(num, junk);
}

char space[400];

void addnum2buff (int *buff, struct TValue *num) __attribute__((__noinline__));
void addnum2buff (int *buff, struct TValue *num) {
  *buff += tostringbuff(num, space);
}

int __attribute__((noipa)) check_space (char *s)
{
  return (s[0] == '1' && s[1] == '.' && s[2] =='0' && s[3] == '\0');
}

int main(void) {
    int buff = 0;
    struct TValue num;
    num.value_.n = 1.0;
    num.tt_ = 19;
    addnum2buff(&buff, &num);
    if (!check_space(space))
      __builtin_abort ();
    return 0;
}
