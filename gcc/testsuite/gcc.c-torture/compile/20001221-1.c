/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */
static void
foo ()
{
  long maplength;
  int type;
  {
    const long nibbles = 8;
    char buf1[nibbles + 1];
    char buf2[nibbles + 1];
    char buf3[nibbles + 1];
    buf1[nibbles] = '\0';
    buf2[nibbles] = '\0';
    buf3[nibbles] = '\0';
    ((nibbles) <= 16
     ? (({
       void *__s = (buf1);
       union
	 {
	   unsigned int __ui;
	   unsigned short int __usi;
	   unsigned char __uc;
	 }
       *__u = __s;
       unsigned char __c = (unsigned char)('0');
       switch ((unsigned int) (nibbles))
	 {
	  case 16:
	   __u->__ui = __c * 0x01010101;
	   __u = __extension__ ((void *) __u + 4);
	  case 12:
	   __u->__ui = __c * 0x01010101;
	   __u = __extension__ ((void *) __u + 4);
	  case 0:
	   break;
	 }
       __s;
     }))
     : 0);
    ((nibbles) <= 16
     ? (({
       void *__s = (buf2);
       union
	 {
	   unsigned int __ui;
	   unsigned short int __usi;
	   unsigned char __uc;
	 }
       *__u = __s;
       unsigned char __c = (unsigned char)('0');
       switch ((unsigned int) (nibbles))
	 {
	  case 16:
	   __u->__ui = __c * 0x01010101;
	   __u = __extension__ ((void *) __u + 4);
	  case 12:
	   __u->__ui = __c * 0x01010101;
	   __u = __extension__ ((void *) __u + 4);
	  case 8:
	   __u->__ui = __c * 0x01010101; 
	   __u = __extension__ ((void *) __u + 4);
	  case 4:
	   __u->__ui = __c * 0x01010101;
	  case 0:
	   break;
	 }
       __s;
     }))
     : 0);
  }
}
