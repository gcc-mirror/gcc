# include <stdio.h>
 
int main(argc, argv, envp)
int argc;
char ** argv;
char ** envp;
{
    int i;
    
    for (i = 1; i < argc; i++) {
    	printf("gc/%s ", argv[i]);
    }
    return(0);
}
