/* { dg-do compile } */
/* { dg-options "" } */

struct obj {
  int n;
  int l;
};
int main()
{
  (struct obj *)((char *)(__SIZE_TYPE__)({ 0; }) - (char *)&((struct obj *)0)->l);
}
