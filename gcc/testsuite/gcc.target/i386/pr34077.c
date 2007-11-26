/* { dg-do compile } */
/* { dg-options "-O1 -minline-all-stringops -minline-stringops-dynamically" } */

#include <string.h>

extern double ran(void);

struct spec_fd_t {
  int limit;
  int len;
  int pos;
  unsigned char *buf;
} spec_fd[3];

int spec_random_load (int fd) {
  int i, j;
  char random_text[(32)][(128*1024)];

  for (j = 0; j < (128*1024); j++) {
    random_text[i][j] = (int)(ran()*256);
  }

  for (i = 0 ; i < spec_fd[fd].limit; i+= (128*1024)) {
    memcpy(spec_fd[fd].buf + i, random_text[(int)(ran()*(32))],
	   (128*1024));
  }

  spec_fd[fd].len = 1024*1024;
  return 0;
}
