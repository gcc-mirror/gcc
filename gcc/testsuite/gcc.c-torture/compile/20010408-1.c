extern struct win *windows, *wtab[];
struct win
{
  struct win *w_next;
};

struct auser;

struct comm
{
  char *name;
  int flags;
};

extern struct comm comms[];

extern int WindowByNoN (char *);
extern int FindCommnr (char *);
extern int AclSetPermCmd (struct auser *, char *, struct comm *);
extern int AclSetPermWin (struct auser *, struct auser *, char *, struct win *);


int
  AclSetPerm(uu, u, mode, s)
    struct auser *uu, *u;
char *mode, *s;
{
  struct win *w;
  int i;
  char *p, ch;

  do 
    {
    }
  while (0);

  while (*s)
    {
      switch (*s)
	{  
	case '*':
	  return AclSetPerm(uu, u, mode, "#?");
	case '#':
	  if (uu)
	    AclSetPermWin(uu, u, mode, (struct win *)1);
	  else
	    for (w = windows; w; w = w->w_next)
	      AclSetPermWin((struct auser *)0, u, mode, w);
	  s++;
	  break;
	case '?':
	  if (uu)
	    AclSetPermWin(uu, u, mode, (struct win *)0);
	  else
	    for (i = 0; i <= 174; i++)
	      AclSetPermCmd(u, mode, &comms[i]);
	  s++;
	  break;
	default:
	  for (p = s; *p && *p != ' ' && *p != '\t' && *p != ','; p++)
	    ;
	  if ((ch = *p))
	    *p++ = '\0';
	  if ((i = FindCommnr(s)) != -1)
	    AclSetPermCmd(u, mode, &comms[i]);
	  else if (((i = WindowByNoN(s)) >= 0) && wtab[i])
	    AclSetPermWin((struct auser *)0, u, mode, wtab[i]);
	  else
	    return -1;
	  if (ch)
	    p[-1] = ch;
	  s = p;
	}
    }

  return 0;
}
