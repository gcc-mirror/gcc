// Build don't link: 
// GROUPS passed unions
union alan {
int a;
char *b;
alan();
};
 
alan mary;
 
alan::alan()
{
        a=0;
}
