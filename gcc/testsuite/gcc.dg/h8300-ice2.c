/* { dg-do compile } */
/* ICE for signed/unsigned long and signed char comparison */
int main()
{
       unsigned long ul = 4;
       long sl = 2;
       signed char sch = -1;
       if (ul <= sch);
               return 0;
       if (sl <= sch)
               return 1;
}
