/* PR 36513: -Wlogical-op warns about strchr */
/* { dg-do compile } */
/* { dg-options "-Wlogical-op" } */

extern void *__rawmemchr (const void *__s, int __c);
int main1 ()
{
  char *s, t;
  (__extension__ (__builtin_constant_p (t) 
		  && !__builtin_constant_p (s) 
		  && (t) == '\0' 
		  ? (char *) __rawmemchr (s, t) 
		  : __builtin_strchr (s, t)));
}

