#include <stdio.h>
  
int debug;

int opencfgfile(const char *cfgfile, FILE **fd)
{
  if (cfgfile[0] != '\0') {

    if ((*fd = fopen(cfgfile, "r")) != NULL) {
      if (debug)
	printf("Config file: --config\n");
    }
    
  }

  return 2;
}
