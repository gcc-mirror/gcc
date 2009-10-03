/* { dg-lto-do assemble } */
void foo(void) {
 char *bar;
 int baz;
 while (1)
   {
     if (baz)
       {
         baz = -baz;
         do
           *bar++ = 0;
         while (++baz);
       }
     ++baz;
   }
}
