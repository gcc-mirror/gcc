/* The strstr call is expanded to just "s", which should not result in a
   warning about discarding qualifiers in an assignment.  */
/* { dg-do compile } */
extern char * strstr (const char *s1, const char * s2);
void foo(const char *s){
  char * cp;
  cp = strstr(s, "");
}
