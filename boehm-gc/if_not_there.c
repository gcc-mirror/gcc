/* Conditionally execute a command based if the file argv[1] doesn't exist */
/* Except for execvp, we stick to ANSI C.				   */
# include "gcconfig.h"
# include <stdio.h>

int main(argc, argv, envp)
int argc;
char ** argv;
char ** envp;
{
    FILE * f;
    if (argc < 3) goto Usage;
    if ((f = fopen(argv[1], "rb")) != 0
        || (f = fopen(argv[1], "r")) != 0) {
        fclose(f);
        return(0);
    }
    printf("^^^^Starting command^^^^\n");
    execvp(argv[2], argv+2);
    exit(1);
    
Usage:
    fprintf(stderr, "Usage: %s file_name command\n", argv[0]);
    return(1);
}

