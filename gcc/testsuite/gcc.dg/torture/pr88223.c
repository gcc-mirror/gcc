/* { dg-do run } */

extern void *memmove(void *, const void *, __SIZE_TYPE__);
extern void abort(void);

extern int
main(void)
{
 char s[] = "12345";
 memmove(s + 1, s, 4);
 memmove(s + 1, s, 4);
 memmove(s + 1, s, 4);
 if (s[0] != '1' || s[1] != '1' || s[2] != '1' || s[3] != '1' || s[4] != '2')
   abort ();
 return (0);
}
