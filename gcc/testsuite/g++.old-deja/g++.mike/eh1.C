// Build don't link:
// Special g++ Options: -fexceptions -O -S

extern "C" void printf (char *, ...);
extern "C" int atoi (const char *);
extern "C" void exit (int);

struct Exception
 {
     int v;
     Exception(int i) { v = i; };
 };

 void inc(int &i)
 {
     try {
         if (i == 0)
             throw Exception(i);
         else
             i++;
     }
     catch (Exception v) {
         i = v.v;
     }
 }

main (int argc, const char *argv[])
{
  if (argc != 2)
    {
      printf ("usage: a.out <num>\n");
      exit (1);
    }
  int count = atoi (argv[1]);
  inc (count);
  printf ("success\n");
  exit (0);
}
