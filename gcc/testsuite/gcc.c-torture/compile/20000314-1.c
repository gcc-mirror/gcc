struct {
   char a[5];
} *p;

int main ()
{
   int i = -1;
   
   if(p->a[-i])
     return 1;
}
