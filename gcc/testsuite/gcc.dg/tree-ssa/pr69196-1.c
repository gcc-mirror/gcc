/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */

/* { dg-final { scan-tree-dump "FSM did not thread around loop and would copy too many statements" "vrp1" } } */


typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
extern void rtems_putc(char c);

void vprintk(
  const char *fmt,
  va_list ap
)
{
  for (; *fmt != '\0'; fmt++) {
    unsigned base = 0;
    unsigned width = 0;
    enum {
      LFLAG_INT,
      LFLAG_LONG,
      LFLAG_LONG_LONG
    } lflag = LFLAG_INT;
    _Bool minus = 0;
    _Bool sign = 0;
    char lead = ' ';
    char c = *fmt;
    long long num;

    if (c != '%') {
      rtems_putc(c);
      continue;
    }

    ++fmt; c = *fmt;

    if (c == '0') {
      lead = '0';
      ++fmt; c = *fmt;
    }

    if (c == '-') {
      minus = 1;
      ++fmt; c = *fmt;
    }

    while (c >= '0' && c <= '9' ) {
      width *= 10;
      width += ((unsigned) c - '0');
      ++fmt; c = *fmt;
    }

    if (c == 'l') {
      lflag = LFLAG_LONG;
      ++fmt; c = *fmt;

      if (c == 'l') {
        lflag = LFLAG_LONG_LONG;
        ++fmt; c = *fmt;
      }
    }

    if ( c == 'c' ) {

      char chr = (char) __builtin_va_arg(ap,int);
      rtems_putc(chr);
      continue;
    }

    if ( c == 's' ) {
      unsigned i, len;
      char *s, *str;

      str = __builtin_va_arg(ap,char *);

      if ( str == ((void *)0) ) {
        str = "";
      }


      for ( len=0, s=str ; *s ; len++, s++ )
        ;


      if ( !minus )
        for ( i=len ; i<width ; i++ )
          rtems_putc(' ');


      if (width == 0) {
          width = len;
      }


      for ( i=0 ; i<width && *str ; str++ )
        rtems_putc(*str);


      if ( minus )
        for ( i=len ; i<width ; i++ )
          rtems_putc(' ');

      continue;
    }


    if ( c == 'o' || c == 'O' ) {
      base = 8; sign = 0;
    } else if ( c == 'i' || c == 'I' ||
                c == 'd' || c == 'D' ) {
      base = 10; sign = 1;
    } else if ( c == 'u' || c == 'U' ) {
      base = 10; sign = 0;
    } else if ( c == 'x' || c == 'X' ) {
      base = 16; sign = 0;
    } else if ( c == 'p' ) {
      base = 16; sign = 0; lflag = LFLAG_LONG;
    } else {
      rtems_putc(c);
      continue;
    }

    switch (lflag) {
      case LFLAG_LONG:
        num = sign ? (long long) __builtin_va_arg(ap,long)
          : (long long) __builtin_va_arg(ap,unsigned long);
        break;
      case LFLAG_LONG_LONG:
        num = __builtin_va_arg(ap,long long);
        break;
      case LFLAG_INT:
      default:
        num = sign ? (long long) __builtin_va_arg(ap,int)
          : (long long) __builtin_va_arg(ap,unsigned int);
        break;
    }
  }
}
