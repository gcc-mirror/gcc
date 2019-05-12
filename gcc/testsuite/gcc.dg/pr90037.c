/* { dg-do compile } */
/* { dg-options "-O2 -Wnull-dereference" } */

typedef __SIZE_TYPE__ size_t;
typedef unsigned long int uintmax_t;

struct group
{
  char *gr_name;
  char *gr_passwd;
  unsigned gr_gid;
  char **gr_mem;
};

struct passwd
{
  char *pw_name;
  char *pw_passwd;

  unsigned pw_uid;
  unsigned pw_gid;
  char *pw_gecos;
  char *pw_dir;
  char *pw_shell;
};

extern struct group *getgrnam (const char *);
extern struct group *getgrgid (unsigned);
extern void endgrent (void);
extern struct passwd *getpwnam (const char *);
extern void endpwent (void);
extern unsigned long int strtoul (const char *__restrict,
				  char **__restrict, int);

char const *
parse_with_separator (char const *spec, char const *separator,
                      unsigned *uid, unsigned *gid,
                      char **username, char **groupname)
{
  static const char *E_invalid_user = "invalid user";
  static const char *E_invalid_group = "invalid group";
  static const char *E_bad_spec = "invalid spec";
  const char *error_msg;
  struct passwd *pwd;
  struct group *grp;
  char *u;
  char const *g;
  char *gname = 0;
  unsigned unum = *uid;
  unsigned gnum = gid ? *gid : (unsigned)-1;

  error_msg = 0;

  if (username)
    *username = 0;

  if (groupname)
    *groupname = 0;

  u = 0;
  if (separator == 0)
    {
      if (*spec)
        u = __builtin_strdup (spec);
    }
  else
    {
      size_t ulen = separator - spec;
      if (ulen != 0)
        {
          u = __builtin_malloc (ulen + 1);
          __builtin_memcpy (u, spec, ulen + 1);
          u[ulen] = '\0';
        }
    }

  g = (separator == 0 || *(separator + 1) == '\0' ? 0 : separator + 1);

  if (u != 0)
    {
      pwd = (*u == '+' ? 0 : getpwnam (u));
      if (pwd == 0)
        {
	  _Bool use_login_group = (separator != 0 && g == 0);
          if (use_login_group)
            {
              error_msg = E_bad_spec;
            }
          else
            {
              unsigned long int tmp;
              tmp = strtoul (u, 0, 10);
              if (tmp <= (1ul << 31) && (unsigned) tmp != (unsigned) -1)
                unum = tmp;
              else
                error_msg = E_invalid_user;
            }
        }
      else
        {
          unum = pwd->pw_uid;
          if (g == 0 && separator != 0)
            {
              char buf[128];
              gnum = pwd->pw_gid;
              grp = getgrgid (gnum);

              gname = buf;

              if (grp)
		gname = __builtin_strdup (grp->gr_name);
              else
		__builtin_snprintf (buf, sizeof(buf), "%ju", (uintmax_t)gnum);

              endgrent ();
            }
        }

      endpwent ();
    }

  if (g != 0 && error_msg == 0)
    {
      grp = (*g == '+' ? 0 : getgrnam (g));
      if (grp == 0)
        {
	  unsigned long int tmp = strtoul (g, 0, 10);
		
	  if (tmp <= (1ul << 31) && (unsigned) tmp != (unsigned) -1)
            gnum = tmp;
          else
            error_msg = E_invalid_group;
        }
      else
        gnum = grp->gr_gid;
      endgrent ();
      gname = __builtin_strdup (g);
    }

  if (error_msg == 0)
    {
      *uid = unum;
      if (gid)
        *gid = gnum;
      if (username)
        {
          *username = u;
          u = 0;
        }
      if (groupname)
        {
          *groupname = gname;
          gname = 0;
        }
    }

  __builtin_free (u);
  __builtin_free (gname);
  return error_msg ? error_msg : 0;
}

