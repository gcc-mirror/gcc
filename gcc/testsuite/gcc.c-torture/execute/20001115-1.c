extern void abort (void);
extern void exit (int);

union iso_directory_record {
   char carr[4];
   struct {
           unsigned char name_len [1];
           char name [0];
   } u;
} entry;

void set(union iso_directory_record *);

int main (void)
{
   union iso_directory_record *de;

   de = &entry;
   set(de);

   if (de->u.name_len[0] == 1 && de->u.name[0] == 0)
     exit (0);
   else
     abort ();
}

void set (union iso_directory_record *p)
{
   p->carr[0] = 1;
   p->carr[1] = 0;
   return;
}
