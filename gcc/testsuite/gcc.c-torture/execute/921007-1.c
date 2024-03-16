void abort(void);
void exit(int);
static int strcmp(){return-1;}
#define strcmp __builtin_strcmp
int
main(void)
{
if(strcmp("X","X\376")>=0)abort();
exit(0);
}
