extern void abort (void);
extern void exit (int);

struct iso_directory_record {
           unsigned char name_len [1];
           char name [0];
} entry;

void set(struct iso_directory_record *);

int main (void)
{
   struct iso_directory_record *de;

   de = &entry;
   set(de);

   if (de->name_len[0] == 1 && de->name[0] == 0)
     exit (0);
   else
     abort ();
}

void set (struct iso_directory_record *p)
{
   p->name_len[0] = 1;
   return;
}
