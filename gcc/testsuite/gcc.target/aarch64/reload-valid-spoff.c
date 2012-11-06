/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=large -fno-builtin" }  */
/* { dg-skip-if "-mcmodel=large -fPIC not currently supported" { aarch64-*-* }  { "-fPIC" } { "" } } */

typedef long unsigned int size_t;
typedef unsigned short int sa_family_t;

struct sockaddr
{
  sa_family_t sa_family;
  char sa_data[14];
};
struct arpreq
{
  int arp_flags;
  struct sockaddr arp_netmask;
};
typedef struct _IO_FILE FILE;
extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream);
extern struct _IO_FILE *stderr;
extern int optind;
struct aftype {
  int (*input) (int type, char *bufp, struct sockaddr *);
};
struct aftype *ap;
static int arp_set(char **args)
{
  char host[128];
  struct arpreq req;
  struct sockaddr sa;
  memset((char *) &req, 0, sizeof(req));
  if (*args == ((void *)0)) {
    fprintf(stderr, ("arp: need host name\n"));
  }
  safe_strncpy(host, *args++, (sizeof host));
  if (ap->input(0, host, &sa) < 0) {
  }
  while (*args != ((void *)0)) {
    if (!__extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (*args) && __builtin_constant_p ("netmask") && (__s1_len = strlen (*args), __s2_len = strlen ("netmask"), (!((size_t)(const void *)((*args) + 1) - (size_t)(const void *)(*args) == 1) || __s1_len >= 4) && (!((size_t)(const void *)(("netmask") + 1) - (size_t)(const void *)("netmask") == 1) || __s2_len >= 4)) ? __builtin_strcmp (*args, "netmask") : (__builtin_constant_p (*args) && ((size_t)(const void *)((*args) + 1) - (size_t)(const void *)(*args) == 1) && (__s1_len = strlen (*args), __s1_len < 4) ? (__builtin_constant_p ("netmask") && ((size_t)(const void *)(("netmask") + 1) - (size_t)(const void *)("netmask") == 1) ? __builtin_strcmp (*args, "netmask") : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("netmask"); register int __result = (((__const unsigned char *) (__const char *) (*args))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (*args))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (*args))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (*args))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p ("netmask") && ((size_t)(const void *)(("netmask") + 1) - (size_t)(const void *)("netmask") == 1) && (__s2_len = strlen ("netmask"), __s2_len < 4) ? (__builtin_constant_p (*args) && ((size_t)(const void *)((*args) + 1) - (size_t)(const void *)(*args) == 1) ? __builtin_strcmp (*args, "netmask") : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (*args); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("netmask"))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("netmask"))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("netmask"))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("netmask"))[3]); } } __result; }))) : __builtin_strcmp (*args, "netmask")))); })) {
      if (__extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (*args) && __builtin_constant_p ("255.255.255.255") && (__s1_len = strlen (*args), __s2_len = strlen ("255.255.255.255"), (!((size_t)(const void *)((*args) + 1) - (size_t)(const void *)(*args) == 1) || __s1_len >= 4) && (!((size_t)(const void *)(("255.255.255.255") + 1) - (size_t)(const void *)("255.255.255.255") == 1) || __s2_len >= 4)) ? __builtin_strcmp (*args, "255.255.255.255") : (__builtin_constant_p (*args) && ((size_t)(const void *)((*args) + 1) - (size_t)(const void *)(*args) == 1) && (__s1_len = strlen (*args), __s1_len < 4) ? (__builtin_constant_p ("255.255.255.255") && ((size_t)(const void *)(("255.255.255.255") + 1) - (size_t)(const void *)("255.255.255.255") == 1) ? __builtin_strcmp (*args, "255.255.255.255") : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("255.255.255.255"); register int __result = (((__const unsigned char *) (__const char *) (*args))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (*args))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (*args))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (*args))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p ("255.255.255.255") && ((size_t)(const void *)(("255.255.255.255") + 1) - (size_t)(const void *)("255.255.255.255") == 1) && (__s2_len = strlen ("255.255.255.255"), __s2_len < 4) ? (__builtin_constant_p (*args) && ((size_t)(const void *)((*args) + 1) - (size_t)(const void *)(*args) == 1) ? __builtin_strcmp (*args, "255.255.255.255") : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (*args); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("255.255.255.255"))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("255.255.255.255"))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("255.255.255.255"))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("255.255.255.255"))[3]); } } __result; }))) : __builtin_strcmp (*args, "255.255.255.255")))); }) != 0) {
	memcpy((char *) &req.arp_netmask, (char *) &sa,
	       sizeof(struct sockaddr));
      }
    }
  }
}
static int arp_file(char *name)
{
  char buff[1024];
  char *sp, *args[32];
  int linenr, argc;
  FILE *fp;
  while (fgets(buff, sizeof(buff), fp) != (char *) ((void *)0)) {
    if (arp_set(args) != 0)
      fprintf(stderr, ("arp: cannot set entry on line %u on line %u of etherfile %s !\n"),
	      linenr, name);
  }
}
int main(int argc, char **argv)
{
  int i, lop, what;
  switch (what) {
  case 0:
    what = arp_file(argv[optind] ? argv[optind] : "/etc/ethers");
  }
}
