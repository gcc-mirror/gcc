int sprintf (char *s, const char *format, ...);

int foo(int i, int j)
{
   char *buf, *str;

   if (i)
     str = "";
   else if (j)
     str = "";
   else
     return 1;

   /* We were propagating &""[0] here and not calling fold_stmt with a
      proper statement pointer.  */
   sprintf(buf, str);
   return 0;
}
